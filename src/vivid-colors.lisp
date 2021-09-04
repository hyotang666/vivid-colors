(in-package :cl-user)

(defpackage :vivid-colors
  (:use :cl)
  (:export ;;;; MAIN API
           "VPRINT" ; like CL:PPRINT
           "*PRINT-VIVID*" ; like CL:*PRINT-PRETTY*
           )
  (:export ;;;; EXTEND
           "SET-VPRINT-DISPATCH" ; like CL:SET-PPRINT-DISPATCH.
           "*VPRINT-DISPATCH*" ; like CL:*PRINT-PPRINT-DISPATCH*.
           "COPY-VPRINT-DISPATCH" ; like CL:COPY-PPRINT-DISPATCH.
           )
  (:export ;;;; EXTEND similar with named-readtables.
           ;;;; MAIN API
           "DEFINE-VPRINT-DISPATCH" ; like CL:DEFPACKAGE.
           "IN-VPRINT-DISPATCH" ; like CL:IN-PACKAGE.
           ;;;; For Hackers.
           "FIND-VPRINT-DISPATCH" ; like CL:FIND-PACKAGE.
           "STORE-VPRINT-DISPATCH" ; like NAMED-READTABLES:REGISTER-READTABLE.
           "MERGE-VPRINT-DISPATCH" ; like
                                   ; NAMED-READTABLES:MERGE-READTABLES-INTO.
           "LIST-ALL-VPRINT-DISPATCHES" ; like CL:LIST-ALL-PACKAGES
           )
  (:export ;;;; HELPER
           "PUT" ; like CL:WRITE.
           "PUT-CHAR" ; like CL:WRITE-CHAR.
           "PUT-STRINGS" ; for partially colored string.
           "VPRINT-NEWLINE" ; like CL:PPRINT-NEWLINE.
           "VPRINT-INDENT" ; like CL:PPRINT-INDENT.
           "VPRINT-POP" ; like CL:PPRINT-POP.
           "VPRINT-EXIT-IF-LIST-EXHAUSTED" ; like
                                           ; CL:PPRINT-EXIT-IF-LIST-EXHAUSTED.
           "VPRINT-LOGICAL-BLOCK" ; like CL:PPRINT-LOGICAL-BLOCK.
           ))

(in-package :vivid-colors)

;;;; CONDITION

(define-condition out-of-scope (program-error cell-error)
  ()
  (:report
   (lambda (this output)
     (funcall
       (formatter "~S is must be in the VPRINT-LOGICAL-BLOCK lexically.")
       output (cell-error-name this)))))

;;;; UTILITIES

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; VPRINT-LOGICAL-BLOCK need this eval-when.
  (defun <stream-var> (designator)
    (typecase designator
      (null '*standard-output*)
      ((eql t) '*terminal-io*)
      (otherwise designator))))

(let ((non-printable-code-point
       (uiop:list-to-hash-set
         (concatenate 'list
                      (loop :for i :upfrom 0 :to #.(char-code #\Space)
                            :collect (code-char i))
                      (string (code-char #x7F))))))
  (defun non-printable-char-p (char)
    (values (gethash char non-printable-code-point))))

;;;; CONFIGURATIONS

(defconstant +default-line-width+ 80)

(declaim (type vprint-dispatch *vprint-dispatch* *standard-vprint-dispatch*))

(defvar *vprint-dispatch*)

(defvar *standard-vprint-dispatch*)

(defvar *vprint-dispatch-repository* (make-hash-table :test #'eq))

(declaim (type boolean *newlinep*))

(defvar *newlinep* nil)

(defvar *vstream*)

(defparameter *print-vivid* t)

;;;; TYPES

(deftype indent () '(integer 0 #.most-positive-fixnum))

(deftype newline-kind () '(member :mandatory :miser :fill :linear nil))

;;; LINE

(defstruct line
  "Elements of queue. Without indentation nor pretty-newline."
  ;; Actual line, including ansi color escape sequence.
  (contents "" :type simple-string :read-only t)
  ;; Line length without ansi color escape sequence.
  (length (error "LENGTH is required.")
          :type (integer 0 #.array-total-size-limit)
          :read-only t)
  ;; Actual indent.
  (indent 0 :type indent))

(defvar *trim-right-p* nil)

(defmethod print-object ((line line) output)
  ;; NOTE: Indent is never printed.
  (cond ((or *print-readably* *print-escape*) (call-next-method))
        (t
         (let ((contents (line-contents line)))
           (write-string contents output
                         :end (when *trim-right-p*
                                (let ((p
                                       (position-if-not #'non-printable-char-p
                                                        contents
                                                        :from-end t)))
                                  (if p
                                      (1+ p)
                                      0))))
           (incf (view-length *vstream*) (line-length line)))))
  line)

;;; QUEUE

(defstruct (queue (:constructor make-queue
                   (&aux (head (cons :head nil)) (tail head))))
  head
  (tail (error "TAIL is required.")))

(defun (setf tail) (new queue)
  (rplacd (queue-tail queue) (setf (queue-tail queue) (list new)))
  new)

(eval-when (:compile-toplevel)
  ;; DOQUEUE need this eval-when.
  (defun count-cons (cons)
    (labels ((rec (cons count)
               (if (atom cons)
                   count
                   (rec (cdr cons) (1+ count)))))
      (rec cons 0))))

(defmacro doqueue ((var <queue> &optional <return>) &body body)
  `(loop :for ,(uiop:ensure-list var) :on (cdr (queue-head ,<queue>))
              :by (lambda (x) (nthcdr ,(count-cons var) x))
         :do (tagbody ,@body)
         :finally (return ,<return>)))

;;; SECTION

(defstruct (section (:conc-name nil))
  ;; Set by PRINCed.
  (start 0 :type indent)
  (prefix "" :type simple-string :read-only t)
  ;; Current indent from start + prefix.
  (indent 0 :type indent)
  (lines (make-queue) :type queue :read-only t)
  (suffix "" :type simple-string :read-only t))

(declaim
 (ftype (function ((or line section))
         (values (mod #.most-positive-fixnum) &optional))
        compute-length))

(defun compute-length (thing)
  (etypecase thing
    (line (line-length thing))
    (section
     (let ((sum 0))
       (declare (type (mod #.most-positive-fixnum) sum))
       (incf sum (length (prefix thing)))
       (doqueue ((thing nil) (lines thing) (setf sum (max 0 (1- sum))))
         (unless (and (typep thing 'line)
                      (every #'non-printable-char-p (line-contents thing)))
           (incf sum (compute-length thing))
           (incf sum)))
       (incf sum (length (suffix thing)))
       sum))))

(defun mandatory? (section)
  (labels ((rec (section)
             (doqueue ((thing kind) (lines section))
               (when (eq :mandatory kind)
                 (return-from mandatory? t))
               (when (typep thing 'section)
                 (rec thing)))))
    (rec section)))

(defmethod print-object ((s section) output)
  (let ((*trim-right-p*))
    (cond ((or *print-readably* *print-escape*) (call-next-method))
          (t
           (setf (start s) (view-length *vstream*))
           (cond
             ((or (not *print-pretty*)
                  (null *print-right-margin*)
                  (and (not *newlinep*)
                       (<= (compute-length s) *print-right-margin*)
                       (not (mandatory? s))))
              (write-string (prefix s) output)
              (incf (view-length *vstream*) (length (prefix s)))
              (doqueue ((thing nil) (lines s))
                (princ thing output))
              (write-string (suffix s) output)
              (incf (view-length *vstream*) (length (suffix s))))
             (t
              (setf *newlinep* t)
              (labels ((newline (thing kind)
                         (terpri output)
                         (setf (view-length *vstream*)
                                 (+ (start s) (length (prefix s))
                                    (if kind
                                        0
                                        (slot-value thing 'indent))))
                         (dotimes (x (view-length *vstream*))
                           (write-char #\Space output)))
                       (miserp ()
                         (and *print-miser-width*
                              (<= (- *print-right-margin* (start s))
                                  *print-miser-width*)
                              (<= *print-right-margin* (compute-length s))))
                       (put-line (thing &optional kind)
                         (let ((*trim-right-p* t))
                           (princ thing output)
                           (newline thing kind))))
                (write-string (prefix s) output)
                (incf (view-length *vstream*) (length (prefix s)))
                (doqueue ((thing newline-kind . rest) (lines s))
                  (mcase:emcase newline-kind newline-kind
                    ((:mandatory :linear) (put-line thing))
                    (:miser
                      (if (miserp)
                          (progn
                           (setf (view-length *vstream*)
                                   (+ (start s) (length (prefix s))))
                           (put-line thing newline-kind))
                          (princ thing output)))
                    (:fill
                      (if (or (and rest
                                   (< *print-right-margin*
                                      (+ (view-length *vstream*)
                                         (compute-length thing)
                                         (compute-length (car rest)))))
                              (miserp))
                          (put-line thing)
                          (princ thing output)))
                    ((nil) (princ thing output))))
                (write-string (suffix s) output)
                (incf (view-length *vstream*) (length (suffix s))))))))))

;;;; VPRINT-STREAM

(defclass vprint-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((output :type stream
           :initarg :output
           :reader output
           :initform *standard-output*
           :documentation "Underlying actual stream.")
   (buffer :initform (make-array (or *print-right-margin* +default-line-width+)
                                 :fill-pointer 0
                                 :adjustable t
                                 :element-type 'character)
           :reader buffer
           :type string
           :documentation "Line buffer. Note this is never include pretty newline.")
   (view-length :initarg :view-length
                :initform 0
                :accessor view-length
                :type indent
                :documentation "Current view length without indent, i.e. line length.")
   (section :initarg :section
            :type section
            :accessor section
            :documentation "Section block.")))

(defmethod initialize-instance :after
           ((o vprint-stream) &key (prefix "") (suffix "") &allow-other-keys)
  (setf (section o) (make-section :prefix prefix :suffix suffix)))

(defmethod trivial-gray-streams:stream-write-char
           ((s vprint-stream) (c character))
  ;; NOTE: Never (INCF (VIEW-LENGTH *VSTREAM*)) here
  ;; because this method is used for escape sequence too.
  (vector-push-extend c (buffer s))
  c)

#+(or ccl clisp)
(defmethod trivial-gray-streams:stream-line-column ((s vprint-stream)) nil)

(declaim
 (ftype (function (character vprint-stream) (values character &optional))
        put-char))

(defun put-char (char output)
  (write-char char output)
  (incf (view-length output))
  char)

(defmacro with-color ((color &key stream args) &body body)
  (let ((output (gensym "OUTPUT")) (pre (gensym "CONTROL-SEQUENCE-PRE")))
    `(let* ((cl-ansi-text:*color-mode* :8bit)
            (,pre (apply #'cl-ansi-text:make-color-string ,color ,args))
            (,output ,stream)
            (cl-ansi-text:*enabled* *print-vivid*))
       (when cl-ansi-text:*enabled*
         (princ ,pre ,output))
       (unwind-protect (progn ,@body (values))
         (when cl-ansi-text:*enabled*
           (princ cl-ansi-text:+reset-color-string+ ,output))))))

(declaim
 (ftype (function (list vprint-stream) (values null &optional)) put-strings))

(defun put-strings (strings output)
  (put-char #\" output)
  (loop :for string-specifier :in strings
        :if (stringp string-specifier)
          :do (write-string string-specifier output)
              (incf (view-length output) (length string-specifier))
        :else :if (typep string-specifier
                         '(cons string (cons cl-ansi-text:color-specifier)))
          :do (destructuring-bind
                  (string color . args)
                  string-specifier
                (with-color (color :stream output :args args)
                  (write-string string output))
                (incf (view-length output) (length string)))
        :else
          :do (error "Unknown string-specifier ~S." string-specifier))
  (put-char #\" output)
  nil)

;;;; PUT

(declaim
 (ftype (function
         (t vprint-stream &key (:color (or null cl-ansi-text:color-specifier))
          (:args list) (:key (or symbol function)))
         (values t &optional))
        put))

(defun put
       (object output
        &key color (key #'prin1-to-string) args
        &aux (key (coerce key 'function)))
  (let ((notation (funcall key object)))
    (declare (type simple-string notation))
    (if color
        (with-color (color :stream output :args args)
          (write-string notation output))
        (write-string notation output))
    (incf (view-length output) (length notation)))
  object)

(declaim
 (ftype (function ((member :block :current) indent vprint-stream) (values))
        vprint-indent))

(defun vprint-indent (kind indent output)
  (let ((section (section output)))
    (setf (indent section)
            (ecase kind (:block indent) (:current (view-length output)))))
  (values))

(declaim
 (ftype (function (newline-kind vprint-stream) (values)) vprint-newline))

(defun vprint-newline (kind output)
  #+clisp
  (progn (check-type kind newline-kind) (check-type output vprint-stream))
  (setf (tail (lines (section output)))
          (make-line :contents (copy-seq (buffer output))
                     :indent (indent (section output))
                     :length (view-length output))
        (tail (lines (section output))) kind
        (fill-pointer (buffer output)) 0
        (view-length output) 0)
  (values))

(defmethod trivial-gray-streams:stream-finish-output ((s vprint-stream))
  (vprint-newline nil s)
  (princ (section s) (output s)))

;;;; DSL

(defmacro vprint-logical-block
          ((<stream-var> <list> &key (prefix "") (suffix "")) &body body)
  (let ((o (gensym "OUTER-MOST-P"))
        (s (gensym "SECTION"))
        (b (gensym "VPRINT-LOGICAL-BLOCK"))
        (l (gensym "LIST"))
        (var (<stream-var> <stream-var>)))
    `(let* ((,l ,<list>)
            (,s
             (when (boundp '*vstream*)
               (section *vstream*)))
            (,var
             (if (boundp '*vstream*)
                 (progn
                  (vprint-newline nil *vstream*)
                  (setf (section *vstream*)
                          (if (not (listp ,l))
                              (make-section)
                              (make-section :prefix ,prefix :suffix ,suffix)))
                  *vstream*)
                 (make-instance 'vprint-stream
                                :output ,var
                                :prefix (if (not (listp ,l))
                                            ""
                                            ,prefix)
                                :suffix (if (not (listp ,l))
                                            ""
                                            ,suffix))))
            (,o (not (boundp '*vstream*)))
            (*vstream* ,var))
       (block ,b
         (unwind-protect
             (if (not (listp ,l))
                 (vprint ,l ,var t)
                 (macrolet ((vprint-pop ()
                              `(if (consp ,',l)
                                   (prog1 (car ,',l) (setf ,',l (cdr ,',l)))
                                   (prog1 ,',l
                                     (put-char #\. ,',var)
                                     (put-char #\Space ,',var)
                                     (setf ,',l nil))))
                            (vprint-exit-if-list-exhausted ()
                              `(unless ,',l
                                 (return-from ,',b (values)))))
                   ,@body))
           (if ,o
               (finish-output ,var)
               (progn
                (vprint-newline nil *vstream*)
                (setf (tail (lines ,s)) (section *vstream*)
                      (tail (lines ,s)) nil
                      (section *vstream*) ,s))))))))

(define-compiler-macro vprint-logical-block
                       (&whole whole
                        (<stream-var> <list> &key (prefix "") (suffix ""))
                        &body body &environment env)
  (if (not (constantp <list> env))
      (progn
       (if (typep <list> '(cons (eql the) (cons (eql list))))
           (let ((o (gensym "OUTER-MOST-P"))
                 (s (gensym "SECTION"))
                 (b (gensym "VPRINT-LOGICAL-BLOCK"))
                 (l (gensym "LIST"))
                 (var (<stream-var> <stream-var>)))
             `(let* ((,l ,<list>)
                     (,s
                      (when (boundp '*vstream*)
                        (section *vstream*)))
                     (,var
                      (if (boundp '*vstream*)
                          (progn
                           (vprint-newline nil *vstream*)
                           (setf (section *vstream*)
                                   (make-section :prefix ,prefix
                                                 :suffix ,suffix))
                           *vstream*)
                          (make-instance 'vprint-stream
                                         :output ,var
                                         :prefix ,prefix
                                         :suffix ,suffix)))
                     (,o (not (boundp '*vstream*)))
                     (*vstream* ,var))
                (block ,b
                  (unwind-protect
                      (macrolet ((vprint-pop ()
                                   `(if (consp ,',l)
                                        (prog1 (car ,',l)
                                          (setf ,',l (cdr ,',l)))
                                        (prog1 ,',l
                                          (put-char #\. ,',var)
                                          (put-char #\Space ,',var)
                                          (setf ,',l nil))))
                                 (vprint-exit-if-list-exhausted ()
                                   `(unless ,',l
                                      (return-from ,',b (values)))))
                        ,@body)
                    (if ,o
                        (finish-output ,var)
                        (progn
                         (vprint-newline nil *vstream*)
                         (setf (tail (lines ,s)) (section *vstream*)
                               (tail (lines ,s)) nil
                               (section *vstream*) ,s)))))))
           whole))
      (let ((<list> (eval <list>)))
        (if <list>
            whole
            (let ((o (gensym "OUTER-MOST-P"))
                  (s (gensym "SECTION"))
                  (b (gensym "VPRINT-LOGICAL-BLOCK"))
                  (var (<stream-var> <stream-var>)))
              `(let* ((,s
                       (when (boundp '*vstream*)
                         (section *vstream*)))
                      (,var
                       (if (boundp '*vstream*)
                           (progn
                            (vprint-newline nil *vstream*)
                            (setf (section *vstream*)
                                    (make-section :prefix ,prefix
                                                  :suffix ,suffix))
                            *vstream*)
                           (make-instance 'vprint-stream
                                          :output ,var
                                          :prefix ,prefix
                                          :suffix ,suffix)))
                      (,o (not (boundp '*vstream*)))
                      (*vstream* ,var))
                 (block ,b
                   (unwind-protect
                       (macrolet ((vprint-pop ()
                                    `(prog1 nil
                                       (put-char #\. ,',var)
                                       (put-char #\Space ,',var)))
                                  (vprint-exit-if-list-exhausted ()
                                    `(return-from ,',b (values))))
                         ,@body)
                     (if ,o
                         (finish-output ,var)
                         (progn
                          (vprint-newline nil *vstream*)
                          (setf (tail (lines ,s)) (section *vstream*)
                                (tail (lines ,s)) nil
                                (section *vstream*) ,s)))))))))))

(defmacro vprint-pop () (error 'out-of-scope :name 'vprint-put))

(defmacro vprint-exit-if-list-exhausted ()
  (error 'out-of-scope :name 'vprint-exit-if-list-exhausted))

;;;; VPRINTER

(defstruct (vprint-dispatch (:copier nil))
  (name (error "NAME is required.") :type symbol)
  (table (make-hash-table :test #'equal) :type hash-table :read-only t))

(defmethod print-object ((this vprint-dispatch) output)
  (cond (*print-readably* (call-next-method))
        (t
         (print-unreadable-object (this output :type t :identity nil)
           (funcall (formatter "~W entry ~D") output
                    (vprint-dispatch-name this)
                    (hash-table-count (vprint-dispatch-table this)))))))

(declaim
 (ftype (function (symbol) (values (or null vprint-dispatch) &optional))
        find-vprint-dispatch))

(defun find-vprint-dispatch (name)
  (values (gethash name *vprint-dispatch-repository*)))

(declaim
 (ftype (function (symbol vprint-dispatch) (values vprint-dispatch &optional))
        store-vprint-dispatch))

(defun store-vprint-dispatch (name vprint-dispatch)
  (setf (gethash name *vprint-dispatch-repository*) vprint-dispatch))

(declaim
 (ftype (function (vprint-dispatch &rest vprint-dispatch)
         (values vprint-dispatch &optional))
        merge-vprint-dispatch))

(defun merge-vprint-dispatch (vprint-dispatch &rest rest)
  (let ((vprint-dispatch (copy-vprint-dispatch vprint-dispatch)))
    (dolist (dispatch rest vprint-dispatch)
      (loop :for key-type :being :each :hash-key :of
                 (vprint-dispatch-table dispatch) :using (:hash-value vprinter)
            :if (gethash key-type (vprint-dispatch-table vprint-dispatch))
              :do (restart-case (error "Duplicates dispatch key.")
                    (replace ()
                        :report "Replace by new one."
                      (setf (gethash key-type
                                     (vprint-dispatch-table vprint-dispatch))
                              vprinter))
                    (ignore () :report "Ignore new one."))
            :else
              :do (setf (gethash key-type
                                 (vprint-dispatch-table vprint-dispatch))
                          vprinter)))))

(defun list-all-vprint-dispatches ()
  (alexandria:hash-table-keys *vprint-dispatch-repository*))

(defmacro define-vprint-dispatch (name &body clause+)
  ;; Trivial syntax check.
  (check-type name symbol)
  (assert (<= (count :merge clause+ :key #'car) 1))
  (assert (every (lambda (clause) (find (car clause) '(:merge :set))) clause+))
  `(let ((*vprint-dispatch*
          ,(let ((merge (find :merge clause+ :key #'car)))
             (if merge
                 `(merge-vprint-dispatch
                    ,@(mapcar (lambda (x) `(find-vprint-dispatch ',x))
                              (cdr merge)))
                 `(make-vprint-dispatch :name ',name)))))
     ,@(loop :for clause :in clause+
             :if (eq :set (car clause))
               :collect `(set-vprint-dispatch ,@(cdr clause)))
     (store-vprint-dispatch ',name *vprint-dispatch*)
     (setf (vprint-dispatch-name *vprint-dispatch*) ',name)))

(defmacro in-vprint-dispatch (name)
  `(setq *vprint-dispatch*
           (or (find-vprint-dispatch ',name)
               (error "Missing VPRINT-DISPATCH named ~S." ',name))))

(defstruct vprinter
  (type (error "TYPE is required.") :read-only t)
  (function (error "FUNCTION is required.")
            :type (or function symbol)
            :read-only t)
  (priority 0 :type real :read-only t))

(declaim
 (ftype (function
         ((or cons symbol) (or symbol function) &optional real vprint-dispatch)
         (values null &optional))
        set-vprint-dispatch))

(defun set-vprint-dispatch
       (type function &optional (priority 0) (table *vprint-dispatch*))
  #+clisp
  (progn (check-type function (or symbol function)) (check-type priority real))
  (assert (millet:type-specifier-p type))
  (remhash type (vprint-dispatch-table table))
  (when function
    (setf (gethash type (vprint-dispatch-table table))
            (make-vprinter :type type :function function :priority priority)))
  nil)

(defun default-printer (output exp) (put exp output) (values))

(declaim
 (ftype (function (t &optional vprint-dispatch)
         (values (or symbol function) boolean &optional))
        vprint-dispatch))

(defun vprint-dispatch (exp &optional (vprint-dispatch *vprint-dispatch*))
  (loop :for key-type :being :each :hash-key :of
             (vprint-dispatch-table vprint-dispatch) :using
             (:hash-value vprinter)
        :if (typep exp key-type)
          :collect vprinter :into vprinters
        :finally (setf vprinters
                         (sort vprinters #'subtypep :key #'vprinter-type))
                 (return
                  (cond ((null vprinters) (values 'default-printer nil))
                        ((null (cdr vprinters)) ; one element.
                         (values (vprinter-function (car vprinters)) t))
                        ((or (and (subtypep (vprinter-type (first vprinters))
                                            (vprinter-type (second vprinters)))
                                  (not
                                    (subtypep
                                      (vprinter-type (second vprinters))
                                      (vprinter-type (first vprinters)))))
                             (not
                               (subtypep (vprinter-type (first vprinters))
                                         (vprinter-type (second vprinters)))))
                         (values (vprinter-function (car vprinters)) t))
                        ((< (vprinter-priority (first vprinters))
                            (vprinter-priority (second vprinters)))
                         (values (vprinter-function (car vprinters)) t))
                        (t
                         (warn "Could not determine vprinters. ~S" vprinters)
                         (values (vprinter-function (car vprinters)) t))))))

(declaim
 (ftype (function (&optional (or null vprint-dispatch))
         (values vprint-dispatch &optional))
        copy-vprint-dispatch))

(defun copy-vprint-dispatch (&optional (vprint-dispatch *vprint-dispatch*))
  (make-vprint-dispatch :name (if vprint-dispatch
                                  (vprint-dispatch-name vprint-dispatch)
                                  :standard)
                        :table (alexandria:copy-hash-table
                                 (if vprint-dispatch
                                     (vprint-dispatch-table vprint-dispatch)
                                     (vprint-dispatch-table
                                       (find-vprint-dispatch :standard))))))

;;;; PRINTERS

(defun vprint-keyword (output keyword)
  (put keyword output :color cl-colors2:+yellow+)
  (values))

(defun vprint-real (output real)
  (put real output :color cl-colors2:+violet+)
  (values))

(defun vprint-symbol (output symbol) (put symbol output) (values))

(defun vprint-string (output string)
  (put string output :color cl-colors2:+tomato+)
  (values))

(defun vprint-char (output char)
  (put char output
       :color cl-colors2:+limegreen+
       :key (lambda (c)
              (if (non-printable-char-p c)
                  (format nil "#\\~A" (char-name c))
                  (prin1-to-string c))))
  (values))

(defun vprint-pathname (output pathname)
  (vprint-logical-block (output nil :prefix "#P")
    (put pathname output
         :color cl-colors2:+tomato+
         :key (lambda (p) (prin1-to-string (namestring p)))))
  (values))

(defun vprint-structure (output structure)
  (vprint-logical-block (output nil :prefix "#S(" :suffix ")")
    (put (type-of structure) output :color cl-colors2:+limegreen+)
    (put-char #\Space output)
    (vprint-indent :current 0 output)
    (loop :for (slot . rest) :on (c2mop:class-slots (class-of structure))
          :for name = (c2mop:slot-definition-name slot)
          :do (put name output
                   :color cl-colors2:+yellow+
                   :key (lambda (n) (format nil ":~S" n)))
              (put-char #\Space output)
              (vprint (slot-value structure name) output t)
              (when rest
                (put-char #\Space output)
                (vprint-newline :linear output)))))

(define-vprint-dispatch :vivid-print-dispatch
  (:set 'keyword 'vprint-keyword)
  (:set 'real 'vprint-real)
  (:set '(and symbol (not keyword)) 'vprint-symbol)
  (:set 'null 'vprint-symbol)
  (:set 'string 'vprint-string)
  (:set 'character 'vprint-char)
  (:set 'pathname 'vprint-pathname)
  (:set 'structure-object 'vprint-structure))

(defun count-pre-body-forms (lambda-list)
  (loop :for elt
             :in (lambda-fiddle:remove-environment-part
                   (lambda-fiddle:remove-whole-part
                     (lambda-fiddle:remove-aux-part lambda-list)))
        :for key? := (find elt lambda-list-keywords)
        :if key?
          :if (eq '&body key?)
            :do (loop-finish)
          :end
        :else
          :count :it))

(defun vprint-macrocall (output form)
  (vprint-logical-block (output form :prefix "(" :suffix ")")
    (vprint (vprint-pop) output t)
    (vprint-exit-if-list-exhausted)
    (vprint-indent :block 3 output)
    (put-char #\Space output)
    (loop :repeat (count-pre-body-forms (millet:lambda-list (car form)))
          :for elt := (vprint-pop)
          :do (vprint-newline :miser output)
              (vprint elt output t)
              (vprint-exit-if-list-exhausted)
              (put-char #\Space output)
          :finally (vprint-indent :block 1 output)
                   (vprint-newline :linear output)
                   ;; body
                   (loop :for elt := (vprint-pop)
                         :do (vprint elt output t)
                             (vprint-exit-if-list-exhausted)
                             (put-char #\Space output)
                             (vprint-newline :linear output)))))

(defun vprint-funcall (output form)
  (vprint-logical-block (output form :prefix "(" :suffix ")")
    (vprint (vprint-pop) output t)
    (vprint-exit-if-list-exhausted)
    (put-char #\Space output)
    (vprint-indent :current 0 output)
    (vprint-newline :miser output)
    (vprint-list output (cdr form) nil :fill)))

(defun vprint-list (output list &optional (print-paren t) (newline-kind :fill))
  (cond
    ((and (symbolp (car list)) (macro-function (car list)) print-paren)
     (vprint-macrocall output list))
    ((and (symbolp (car list)) (fboundp (car list)) print-paren)
     (vprint-funcall output list))
    (t
     (vprint-logical-block (output nil
                                   :prefix (if print-paren
                                               "("
                                               "")
                                   :suffix (if print-paren
                                               ")"
                                               ""))
       (labels ((rec (list)
                  (cond ((null list))
                        ((atom list)
                         (put-char #\. output)
                         (put-char #\Space output)
                         (vprint list output t))
                        ((consp list)
                         (vprint (car list) output t)
                         (when (cdr list)
                           (put-char #\Space output)
                           (vprint-newline newline-kind output)
                           (rec (cdr list)))))))
         (rec list)))))
  (values))

(defun vprint-vector (output vector)
  (vprint-logical-block (output nil :prefix "#(" :suffix ")")
    (do ((i 0 (1+ i)))
        (nil)
      (vprint (aref vector i) output t)
      (if (array-in-bounds-p vector (1+ i))
          (progn (put-char #\Space output) (vprint-newline :fill output))
          (return)))))

(defun vprint-array (output array)
  (labels ((rec (dims indices output)
             (if (endp dims)
                 (vprint (apply #'aref array (reverse indices)) output)
                 (vprint-logical-block (output nil :prefix "(" :suffix ")")
                   (dotimes (x (car dims))
                     (rec (cdr dims) (cons x indices) output)
                     (when (< (1+ x) (car dims))
                       (put-char #\Space output)))))))
    (vprint-logical-block (output nil
                                  :prefix (format nil "#~DA"
                                                  (array-rank array)))
      (rec (array-dimensions array) nil output))))

(defun vprint-quote (output quote)
  (if (cddr quote)
      (vprint-list output quote)
      (vprint-logical-block (output nil :prefix "'")
        (vprint-list output (cdr quote) nil)))
  (values))

(defun vprint-function (output function)
  (if (cddr function)
      (vprint-list output function)
      (vprint-logical-block (output nil :prefix "#'")
        (vprint-list output (cdr function) nil)))
  (values))

(defun vprint-backquote (output backquote)
  (vprint-logical-block (output nil :prefix "`")
    (vprint-list output (cdr backquote) nil))
  (values))

(defun vprint-let (output exp)
  (vprint-logical-block (output exp :prefix "(" :suffix ")")
    (vprint (vprint-pop) output t) ; operator.
    (vprint-exit-if-list-exhausted)
    (put-char #\Space output)
    (vprint-indent :current 0 output)
    (vprint-newline :miser output)
    ;; Binds.
    (vprint-logical-block (output (vprint-pop) :prefix "(" :suffix ")")
      (vprint-exit-if-list-exhausted)
      (loop :for bind = (vprint-pop)
            :if (atom bind)
              :do (vprint bind output t)
                  (vprint-exit-if-list-exhausted)
                  (put-char #\Space output)
            :else
              :do (vprint-logical-block (output (the list bind)
                                                :prefix "("
                                                :suffix ")")
                    (vprint-exit-if-list-exhausted)
                    (vprint (vprint-pop) output t) ; var
                    (vprint-exit-if-list-exhausted)
                    (put-char #\Space output)
                    (vprint-newline :miser output)
                    (loop (vprint (vprint-pop) output t) ; form
                          (vprint-exit-if-list-exhausted)
                          (put-char #\Space output)
                          (vprint-newline :linear output)))
                  (vprint-exit-if-list-exhausted)
                  (put-char #\Space output)))
    ;; Body
    (vprint-exit-if-list-exhausted)
    (put-char #\Space output)
    (vprint-indent :block 1 output)
    (vprint-newline :linear output)
    (loop (vprint (vprint-pop) output t)
          (vprint-exit-if-list-exhausted)
          (put-char #\Space output)
          (vprint-newline :linear output))))

(defun vprint-block (output exp)
  (vprint-logical-block (output exp :prefix "(" :suffix ")")
    (vprint (vprint-pop) output t) ; operator
    (vprint-exit-if-list-exhausted)
    (put-char #\Space output)
    (vprint-indent :block 1 output)
    (vprint-newline :miser output)
    (loop (vprint (vprint-pop) output t)
          (vprint-exit-if-list-exhausted)
          (put-char #\Space output)
          (vprint-newline :linear output))))

(define-vprint-dispatch :pretty-print-dispatch
  (:set 'list 'vprint-list)
  (:set 'vector 'vprint-vector)
  (:set 'array 'vprint-array)
  (:set '(cons (member quote)) 'vprint-quote)
  (:set '(cons (member function)) 'vprint-function)
  (:set '(cons (member #.(or #+sbcl 'sb-int:quasiquote))) 'vprint-backquote)
  (:set '(cons (member let let* symbol-macrolet)) 'vprint-let)
  (:set
   '(cons
      (member block unwind-protect prog1 return-from catch throw eval-when
              multiple-value-call multiple-value-prog1))
   'vprint-block))

(define-vprint-dispatch :standard
  (:merge :vivid-print-dispatch :pretty-print-dispatch))

(setq *vprint-dispatch* (find-vprint-dispatch :standard))

(setq *standard-vprint-dispatch* (copy-vprint-dispatch))

;;;; VPRINT

(declaim
 (ftype (function (t &optional stream boolean) (values null &optional)) vprint))

(defun vprint (exp &optional output recursivep)
  (if recursivep
      (funcall (coerce (vprint-dispatch exp) 'function) output exp)
      (let ((*print-right-margin*
             (or *print-right-margin* +default-line-width+))
            (*print-miser-width* (or *print-miser-width* 60))
            (*newlinep*))
        (vprint-logical-block (output nil)
          (funcall (coerce (vprint-dispatch exp) 'function) output exp)))))