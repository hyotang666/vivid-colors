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
  (:export ;;;; HELPER
           "PUT" ; like CL:WRITE.
           "PUT-CHAR" ; like CL:WRITE-CHAR.
           "VPRINT-NEWLINE" ; like CL:PPRINT-NEWLINE.
           "VPRINT-INDENT" ; like CL:PPRINT-INDENT.
           "VPRINT-LOGICAL-BLOCK" ; like CL:PPRINT-LOGICAL-BLOCK.
           ))

(in-package :vivid-colors)

;;;; UTILITIES

(defun ensure-output-stream (designator)
  (etypecase designator
    (stream designator)
    (null *standard-output*)
    ((eql t) *terminal-io*)))

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

(defparameter *vprint-dispatch* nil)

(defvar *standard-vprint-dispatch*)

(declaim (type boolean *newlinep*))

(defvar *newlinep* nil)

(defvar *vstream*)

(defparameter *print-vivid* t)

;;;; TYPES

(deftype indent () '(integer 0 #.most-positive-fixnum))

(deftype newline-kind ()
  '(member :mandatory :miser :fill :linear :mandatory nil))

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
       (doqueue ((thing nil) (lines thing) (decf sum))
         (unless (and (typep thing 'line)
                      (every #'non-printable-char-p (line-contents thing)))
           (incf sum (compute-length thing))
           (incf sum)))
       (incf sum (length (suffix thing)))
       sum))))

(defmethod print-object ((s section) output)
  (let ((*trim-right-p*))
    (cond ((or *print-readably* *print-escape*) (call-next-method))
          (t
           (setf (start s) (view-length *vstream*))
           (cond
             ((<= (compute-length s) *print-right-margin*)
              (write-string (prefix s) output)
              (incf (view-length *vstream*) (length (prefix s)))
              (doqueue ((thing nil) (lines s))
                (princ thing output))
              (write-string (suffix s) output)
              (incf (view-length *vstream*) (length (suffix s))))
             (t
              (setf *newlinep* t)
              (flet ((newline (thing)
                       (terpri output)
                       (setf (view-length *vstream*)
                               (+ (start s) (length (prefix s))
                                  (slot-value thing 'indent)))
                       (dotimes (x (view-length *vstream*))
                         (write-char #\Space output))))
                (write-string (prefix s) output)
                (incf (view-length *vstream*) (length (prefix s)))
                (doqueue ((thing newline-kind . rest) (lines s))
                  (mcase:emcase newline-kind newline-kind
                    (:mandatory
                      (let ((*trim-right-p* t))
                        (princ thing output))
                      (newline thing))
                    (:linear
                      (let ((*trim-right-p* t))
                        (princ thing output))
                      (newline thing))
                    (:miser
                      (if (and *print-miser-width*
                               (<= *print-miser-width*
                                   (- *print-right-margin* (start s))))
                          (let ((*trim-right-p* t))
                            (princ thing output)
                            (newline thing))
                          (princ thing output)))
                    (:fill
                      (if (or (and rest
                                   (< *print-right-margin*
                                      (+ (start s)
                                         (compute-length (car rest)))))
                              *newlinep*)
                          (let ((*trim-right-p* t))
                            (princ thing output)
                            (newline thing))
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

(declaim
 (ftype (function (character vprint-stream) (values character &optional))
        put-char))

(defun put-char (char output)
  (write-char char output)
  (incf (view-length output))
  char)

(defmacro with-color ((color &key stream) &body body)
  (let ((output (gensym "OUTPUT")) (pre (gensym "CONTROL-SEQUENCE-PRE")))
    `(let* ((cl-ansi-text:*color-mode* :8bit)
            (,pre
             (cl-ansi-text:make-color-string ,color
                                             :effect :unset
                                             :style :foreground))
            (,output ,stream)
            (cl-ansi-text:*enabled* *print-vivid*))
       (when cl-ansi-text:*enabled*
         (princ ,pre ,output))
       (unwind-protect (progn ,@body (values))
         (when cl-ansi-text:*enabled*
           (princ cl-ansi-text:+reset-color-string+ ,output))))))

(declaim
 (ftype (function
         (t vprint-stream &key (:color (or null cl-ansi-text:color-specifier))
          (:key (or symbol function)))
         (values t &optional))
        put))

(defun put
       (object output
        &key color (key #'prin1-to-string)
        &aux (key (coerce key 'function)))
  (let ((notation (funcall key object)))
    (declare (type simple-string notation))
    (if color
        (with-color (color :stream output)
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
  (when (typep output 'vprint-stream)
    (setf (tail (lines (section output)))
            (make-line :contents (copy-seq (buffer output))
                       :indent (indent (section output))
                       :length (view-length output))
          (tail (lines (section output))) kind
          (fill-pointer (buffer output)) 0
          (view-length output) 0))
  (values))

(defmethod trivial-gray-streams:stream-finish-output ((s vprint-stream))
  (vprint-newline nil s)
  (princ (section s) (output s)))

;;;; DSL

(defmacro vprint-logical-block
          ((var <stream> &key (prefix "") (suffix "")) &body body)
  (let ((o (gensym "OUTER-MOST-P")) (s (gensym "SECTION")))
    `(let* ((,s
             (when (boundp '*vstream*)
               (section *vstream*)))
            (,var
             (if (boundp '*vstream*)
                 (progn
                  (vprint-newline nil *vstream*)
                  (setf (section *vstream*)
                          (make-section :prefix ,prefix :suffix ,suffix))
                  *vstream*)
                 (make-instance 'vprint-stream
                                :output (ensure-output-stream ,<stream>)
                                :prefix ,prefix
                                :suffix ,suffix)))
            (,o (not (boundp '*vstream*)))
            (*vstream* ,var))
       (unwind-protect (progn ,@body)
         (if ,o
             (finish-output ,var)
             (progn
              (vprint-newline nil *vstream*)
              (setf (tail (lines ,s)) (section *vstream*)
                    (tail (lines ,s)) nil
                    (section *vstream*) ,s)))))))

;;;; VPRINTER

(defstruct vprinter
  (type (error "TYPE is required.") :read-only t)
  (function (error "FUNCTION is required.")
            :type (or function symbol)
            :read-only t)
  (priority 0 :type real :read-only t))

(declaim
 (ftype (function ((or cons symbol) (or symbol function) &optional real)
         (values (eql t) &optional))
        set-vprint-dispatch))

(defun set-vprint-dispatch (type function &optional (priority 0))
  (push (make-vprinter :type type :function function :priority priority)
        *vprint-dispatch*)
  t)

(defun default-printer (output exp) (put exp output) (values))

(defun vprint-dispatch (exp &optional (dispatch-table *vprint-dispatch*))
  (loop :for vprinter :in dispatch-table
        :if (typep exp (vprinter-type vprinter))
          :collect vprinter :into vprinters
        :finally (setf vprinters
                         (sort vprinters #'subtypep :key #'vprinter-type))
                 (return
                  (cond ((null vprinters) 'default-printer)
                        ((null (cdr vprinters)) ; one element.
                         (vprinter-function (car vprinters)))
                        ((or (and (subtypep (vprinter-type (first vprinters))
                                            (vprinter-type (second vprinters)))
                                  (not
                                    (subtypep
                                      (vprinter-type (second vprinters))
                                      (vprinter-type (first vprinters)))))
                             (not
                               (subtypep (vprinter-type (first vprinters))
                                         (vprinter-type (second vprinters)))))
                         (vprinter-function (car vprinters)))
                        ((< (vprinter-priority (first vprinters))
                            (vprinter-priority (second vprinters)))
                         (vprinter-function (car vprinters)))
                        (t
                         (error "Could not determine vprinters. ~S"
                                vprinters))))))

(declaim
 (ftype (function (&optional (or null cons)) (values list &optional))
        copy-vprint-dispatch))

(defun copy-vprint-dispatch (&optional (vprint-dispatch *vprint-dispatch*))
  (if vprint-dispatch
      (copy-seq vprint-dispatch)
      (copy-seq *standard-vprint-dispatch*)))

;;;; PRINTERS

(defun vprint-keyword (output keyword)
  (put keyword output :color cl-colors2:+yellow+)
  (values))

(set-vprint-dispatch 'keyword 'vprint-keyword)

(defun vprint-real (output real)
  (put real output :color cl-colors2:+violet+)
  (values))

(set-vprint-dispatch 'real 'vprint-real)

(defun vprint-symbol (output symbol) (put symbol output) (values))

(set-vprint-dispatch '(and symbol (not keyword)) 'vprint-symbol)

(set-vprint-dispatch 'null 'vprint-symbol)

(defun vprint-string (output string)
  (put string output :color cl-colors2:+tomato+)
  (values))

(set-vprint-dispatch 'string 'vprint-string)

(defun vprint-char (output char)
  (put char output
       :color cl-colors2:+limegreen+
       :key (lambda (c)
              (if (non-printable-char-p c)
                  (format nil "#\\~A" (char-name c))
                  (prin1-to-string c))))
  (values))

(set-vprint-dispatch 'character 'vprint-char)

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
  (vprint-logical-block (output output :prefix "(" :suffix ")")
    (vprint (first form) output t)
    (when (null (cdr form))
      (return-from vprint-macrocall (values)))
    (vprint-indent :block 3 output)
    (put-char #\Space output)
    (loop :repeat (count-pre-body-forms (millet:lambda-list (car form)))
          :for (elt . rest) :on (cdr form)
          :do (vprint-newline :miser output)
              (vprint elt output t)
              (when (null rest)
                (return-from vprint-macrocall (values)))
              (put-char #\Space output)
          :finally (vprint-indent :block 1 output)
                   (vprint-newline :fill output)
                   ;; body
                   (loop :for (elt . rest) :on rest
                         :do (vprint elt output t)
                             (when (null rest)
                               (return-from vprint-macrocall (values)))
                             (put-char #\Space output)
                             (vprint-newline :fill output)))))

(defun vprint-funcall (output form)
  (vprint-logical-block (output output :prefix "(" :suffix ")")
    (vprint (first form) output t)
    (cond ((null (cdr form)) (values))
          ((atom (cdr form))
           (put-char #\Space output)
           (put-char #\. output)
           (put-char #\Space output)
           (vprint (cdr form) output t))
          (t
           (put-char #\Space output)
           (vprint-indent :current 0 output)
           (vprint-newline :miser output)
           (vprint-list output (cdr form) nil :fill)))))

(defun vprint-list (output list &optional (print-paren t) (newline-kind :fill))
  (cond
    ((and (symbolp (car list)) (macro-function (car list)) print-paren)
     (vprint-macrocall output list))
    ((and (symbolp (car list)) (fboundp (car list)) print-paren)
     (vprint-funcall output list))
    (t
     (vprint-logical-block (output output
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

(set-vprint-dispatch 'list 'vprint-list)

(defun vprint-quote (output quote)
  (if (cddr quote)
      (vprint-list output quote)
      (vprint-logical-block (output output :prefix "'")
        (vprint-list output (cdr quote) nil)))
  (values))

(set-vprint-dispatch '(cons (member quote)) 'vprint-quote)

(defun vprint-function (output function)
  (if (cddr function)
      (vprint-list output function)
      (vprint-logical-block (output output :prefix "#'")
        (vprint-list output (cdr function) nil)))
  (values))

(set-vprint-dispatch '(cons (member function)) 'vprint-function)

(defun vprint-backquote (output backquote)
  (vprint-logical-block (output output :prefix "`")
    (vprint-list output (cdr backquote) nil))
  (values))

(set-vprint-dispatch '(cons (member #.(or #+sbcl 'sb-int:quasiquote)))
                     'vprint-backquote)

(defun vprint-pathname (output pathname)
  (vprint-logical-block (output output :prefix "#P")
    (put pathname output
         :color cl-colors2:+tomato+
         :key (lambda (p) (prin1-to-string (namestring p)))))
  (values))

(set-vprint-dispatch 'pathname 'vprint-pathname)

(defun vprint-structure (output structure)
  (vprint-logical-block (output output :prefix "#S(" :suffix ")")
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

(set-vprint-dispatch 'structure-object 'vprint-structure)

(setq *standard-vprint-dispatch* (copy-vprint-dispatch))

;;;; VPRINT

(declaim
 (ftype (function (t &optional stream boolean) (values null &optional)) vprint))

(defun vprint (exp &optional output recursivep)
  (if recursivep
      (funcall (coerce (vprint-dispatch exp) 'function) output exp)
      (let ((*print-right-margin*
             (or *print-right-margin* +default-line-width+))
            (*newlinep*))
        (vprint-logical-block (output output)
          (funcall (coerce (vprint-dispatch exp) 'function) output exp)))))
