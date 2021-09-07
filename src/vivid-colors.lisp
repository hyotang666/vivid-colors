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

(declaim (type boolean *newlinep*))

(defvar *newlinep* nil)

(defvar *vstream*)

(defparameter *print-vivid* t)

;;;; TYPES

(deftype indent () '(integer 0 #.most-positive-fixnum))

(deftype newline-kind () '(member :mandatory :miser :fill :linear nil))

;;;; GF

(defgeneric compute-length (thing))

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

(defmethod compute-length ((line line)) (line-length line))

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

;;; SECTION

(defstruct (section (:conc-name nil))
  ;; Set by PRINCed.
  (start 0 :type indent)
  (prefix "" :type simple-string :read-only t)
  ;; Current indent from start + prefix.
  (indent 0 :type indent)
  (lines (make-queue) :type queue :read-only t)
  (suffix "" :type simple-string :read-only t))

(defmethod compute-length ((section section))
  (let ((sum 0))
    (declare (type (mod #.most-positive-fixnum) sum))
    (incf sum (length (prefix section)))
    (doqueue ((thing nil) (lines section) (setf sum (max 0 (1- sum))))
      (unless (and (typep thing 'line)
                   (every #'non-printable-char-p (line-contents thing)))
        (incf sum (compute-length thing))
        (incf sum)))
    (incf sum (length (suffix section)))
    sum))

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

;;;; PRINTERS

(defun vprint-keyword (output keyword)
  (put keyword output :color cl-colors2:+yellow+)
  (values))

(defun vprint-real (output real)
  (put real output :color cl-colors2:+violet+)
  (values))

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

(define-vprint-dispatch :vivid
  (:set 'keyword 'vprint-keyword)
  (:set 'real 'vprint-real)
  (:set 'string 'vprint-string)
  (:set 'character 'vprint-char)
  (:set 'pathname 'vprint-pathname))

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
     (vprint-logical-block (output (the list list)
                                   :prefix (if print-paren
                                               "("
                                               "")
                                   :suffix (if print-paren
                                               ")"
                                               ""))
       (vprint-exit-if-list-exhausted)
       (loop (vprint (vprint-pop) output t)
             (vprint-exit-if-list-exhausted)
             (put-char #\Space output)
             (vprint-newline newline-kind output)))))
  (values))

(defun vprint-vector (output vector)
  (if (typep vector 'string)
      (default-printer output vector)
      (vprint-logical-block (output nil :prefix "#(" :suffix ")")
        (do ((i 0 (1+ i)))
            (nil)
          (vprint (aref vector i) output t)
          (if (array-in-bounds-p vector (1+ i))
              (progn (put-char #\Space output) (vprint-newline :fill output))
              (return))))))

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

(defun vprint-structure (output structure)
  (vprint-logical-block (output nil :prefix "#S(" :suffix ")")
    (vprint (type-of structure) output)
    (put-char #\Space output)
    (vprint-indent :current 0 output)
    (loop :for (slot . rest) :on (c2mop:class-slots (class-of structure))
          :for name = (c2mop:slot-definition-name slot)
          :do (vprint (intern (symbol-name name) :keyword) output t)
              (put-char #\Space output)
              (vprint (slot-value structure name) output t)
              (when rest
                (put-char #\Space output)
                (vprint-newline :linear output)))))

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

(defun vprint-if (output exp)
  (vprint-logical-block (output exp :prefix "(" :suffix ")")
    (vprint (vprint-pop) output t) ; operator
    (vprint-exit-if-list-exhausted)
    (put-char #\Space output)
    (vprint-indent :current 0 output)
    (loop (vprint (vprint-pop) output t)
          (vprint-exit-if-list-exhausted)
          (put-char #\Space output)
          (vprint-newline :linear output))))

(defun vprint-progn (output exp)
  (vprint-logical-block (output exp :prefix "(" :suffix ")")
    (vprint (vprint-pop) output t) ; operator
    (vprint-exit-if-list-exhausted)
    (put-char #\Space output)
    (vprint-indent :block 1 output)
    (vprint-newline :linear output)
    (loop (vprint (vprint-pop) output t)
          (vprint-exit-if-list-exhausted)
          (put-char #\Space output)
          (vprint-newline :linear output))))

(define-vprint-dispatch :pretty
  (:set 'null 'default-printer)
  (:set 'list 'vprint-list)
  (:set 'vector 'vprint-vector)
  (:set 'array 'vprint-array)
  (:set 'structure-object 'vprint-structure)
  (:set '(cons (member quote)) 'vprint-quote)
  (:set '(cons (member function)) 'vprint-function)
  (:set '(cons (member #.(or #+sbcl 'sb-int:quasiquote))) 'vprint-backquote)
  (:set '(cons (member let let* symbol-macrolet)) 'vprint-let)
  (:set '(cons (member if)) 'vprint-if)
  (:set '(cons (member progn locally tagbody)) 'vprint-progn)
  (:set
   '(cons
      (member block unwind-protect prog1 return-from catch throw eval-when
              multiple-value-call multiple-value-prog1))
   'vprint-block))

(define-vprint-dispatch :standard
  (:merge :vivid :pretty))

(setq *vprint-dispatch* (find-vprint-dispatch :standard))

;;;; VPRINT

(declaim
 (ftype (function (t &optional stream boolean) (values null &optional)) vprint))

(defun vprint (exp &optional (output *standard-output*) recursivep)
  (if recursivep
      (funcall (coerce (vprint-dispatch exp) 'function) output exp)
      (let ((*print-right-margin*
             (or *print-right-margin* +default-line-width+))
            (*print-miser-width* (or *print-miser-width* 60))
            (*newlinep*))
        (vprint-logical-block (output nil)
          (funcall (coerce (vprint-dispatch exp) 'function) output exp)))))