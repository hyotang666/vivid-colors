(in-package :cl-user)

(defpackage :vivid-colors
  (:use :cl)
  (:export))

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

;;;; TYPES

(deftype indent () '(integer 0 #.most-positive-fixnum))

(deftype newline-kind () '(member :mandatory :miser :fill :linear :mandatory))

;;; LINE

(defstruct line
  "Elements of queue. Without indentation nor pretty-newline."
  ;; Actual line, including ansi color escape sequence.
  (contents "" :type simple-string :read-only t)
  ;; Line length without ansi color escape sequence.
  (length (error "LENGTH is required.")
          :type (integer 0 #.array-total-size-limit)
          :read-only t)
  ;; Ending newline kind.
  (break (error "BREAK is required.") :type newline-kind :read-only t))

(defmethod print-object ((line line) output)
  (cond ((or *print-readably* *print-escape*) (call-next-method))
        (t
         (let ((p
                (position-if-not #'non-printable-char-p (line-contents line)
                                 :from-end t)))
           (write-string (line-contents line) output :end (and p (1+ p)))))))

;;;; CONFIGURATIONS

(defconstant +default-line-width+ 80)

(defparameter *vprint-dispatch* nil)

(declaim (type indent *position*))

(defvar *position*)

(defmethod documentation ((o (eql '*position*)) (type (eql 'variable)))
  "Current view position from the outer most vstream.")

(declaim (type boolean *newlinep*))

(defvar *newlinep* nil)

;;; QUEUE

(defstruct (queue (:constructor make-queue
                   (&aux (head (cons :head nil)) (tail head))))
  head
  (tail (error "TAIL is required.")))

(defun (setf tail) (new queue)
  (rplacd (queue-tail queue) (setf (queue-tail queue) (list new)))
  new)

(defmacro doqueue ((var <queue> &optional <return>) &body body)
  `(loop :for ,(uiop:ensure-list var) :on (cdr (queue-head ,<queue>))
         :do (tagbody ,@body)
         :finally (return ,<return>)))

;;;; VPRINTER

(defstruct vprinter
  (type (error "TYPE is required.") :read-only t)
  (function (error "FUNCTION is required.")
            :type (or function symbol)
            :read-only t)
  (priority 0 :type real :read-only t))

(defun set-vprint-dispatch (type function &optional (priority 0))
  (push (make-vprinter :type type :function function :priority priority)
        *vprint-dispatch*)
  t)

(defun default-printer (output exp)
  (let ((representation (prin1-to-string exp)))
    (write-string representation output)
    (incf *position* (length representation)))
  (values))

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
           :documentation "Line buffer. Note this is never include pretty newline.")
   (start :initarg :start
          :reader start
          :type indent
          :documentation "Start position of this block.")
   (indent :initarg :indent
           :accessor indent
           :documentation "Indent from the start position.")
   (queue-head :initform (cons :head nil)
               :reader head
               :documentation "Storing LINE objects.")
   (queue-tail :reader tail)
   (prefix :initform "" :initarg :prefix :reader prefix)
   (suffix :initform "" :initarg :suffix :reader suffix)))

(defmethod initialize-instance :after ((o vprint-stream) &rest args)
  (declare (ignore args))
  (setf (indent o) (length (prefix o))
        (slot-value o 'queue-tail) (head o)))

(defmethod trivial-gray-streams:stream-write-char
           ((s vprint-stream) (c character))
  (vector-push-extend c (buffer s))
  c)

(defun vprint-indent (kind indent output)
  (setf (indent output)
          (ecase kind
            (:block indent)
            (:current
             (+ indent
                (- *position* (start output) (length (prefix output)))))))
  (values))

(defun compute-line-length (vstream)
  (- *position* (length (prefix vstream)) (indent vstream) (start vstream)))

(declaim
 (ftype (function (newline-kind vprint-stream) (values)) vprint-newline))

(defun vprint-newline (kind output)
  (when (typep output 'vprint-stream)
    (setf (tail output)
            (make-line :contents (copy-seq (buffer output))
                       :length (compute-line-length output)
                       :break kind)
          (fill-pointer (buffer output)) 0))
  (values))

(defun compute-block-total-length (vstream)
  (+ (length (prefix vstream))
     (let ((sum -1))
       (doqueue (line vstream (max 0 sum))
         (incf sum (1+ (line-length line)))))
     (fill-pointer (buffer vstream)) (length (suffix vstream))))

(defmethod trivial-gray-streams:stream-finish-output ((s vprint-stream))
  (cond
    ((< (+ *position* (compute-block-total-length s)) *print-right-margin*)
     (write-string (prefix s) (output s))
     (doqueue (line s)
       (princ line (output s))
       (write-char #\Space (output s)))
     (write-string (buffer s) (output s))
     (write-string (suffix s) (output s)))
    (t
     (flet ((newline ()
              (setf *newlinep* t)
              (terpri (output s))
              (dotimes (x (indent s)) (write-char #\Space (output s)))))
       (write-string (prefix s) (output s))
       (doqueue ((line . rest) s)
         (princ line (output s))
         (write-char #\Space (output s))
         (mcase:emcase newline-kind (line-break line)
           (:mandatory (newline))
           (:linear (newline))
           (:miser
             (when (and *print-miser-width*
                        (<= *print-miser-width*
                            (- *print-right-margin* *position*)))
               (newline)))
           (:fill
             (when (or (and rest
                            (< *print-right-margin*
                               (+ *position* (line-length (car rest)))))
                       *newlinep*)
               (newline)))))
       (write-string (buffer s) (output s))
       (write-string (suffix s) (output s))))))

;;;; DSL

(defmacro with-color ((color &key stream) &body body)
  (let ((output (gensym "OUTPUT")) (pre (gensym "CONTROL-SEQUENCE-PRE")))
    `(let* ((cl-ansi-text:*color-mode* :8bit)
            (,pre
             (cl-ansi-text:make-color-string ,color
                                             :effect :unset
                                             :style :foreground))
            (,output ,stream))
       (when cl-ansi-text:*enabled*
         (princ ,pre ,output)
         (unwind-protect
             (progn
              ,@body
              (incf *print-right-margin*
                    (+ (length ,pre)
                       (length cl-ansi-text:+reset-color-string+)))
              (values))
           (when cl-ansi-text:*enabled*
             (princ cl-ansi-text:+reset-color-string+ ,output)))))))

(defmacro vprint-logical-block
          ((var <stream> &key (prefix "") (suffix "")) &body body)
  (let ((p (gensym "PREFIX")))
    `(let* ((,p ,prefix)
            (,var
             (make-instance 'vprint-stream
                            :output (ensure-output-stream ,<stream>)
                            :prefix ,p
                            :suffix ,suffix
                            :start *position*))
            (*position* (+ *position* (length ,p))))
       (unwind-protect (progn ,@body) (finish-output ,var)))))

;;;; PRINTERS

(defun vprint-keyword (output keyword)
  (with-color (cl-colors2:+yellow+ :stream output)
    (prin1 keyword output))
  (incf *position* (1+ (length (symbol-name keyword))))
  (values))

(set-vprint-dispatch 'keyword 'vprint-keyword)

(defun vprint-real (output real)
  (let ((representation (prin1-to-string real)))
    (with-color (cl-colors2:+violet+ :stream output)
      (write-string representation output))
    (incf *position* (length representation))
    (values)))

(set-vprint-dispatch 'real 'vprint-real)

(defun vprint-symbol (output symbol)
  (let ((representation (prin1-to-string symbol)))
    (write-string representation output)
    (incf *position* (length representation)))
  (values))

(set-vprint-dispatch '(and symbol (not keyword)) 'vprint-symbol)

(set-vprint-dispatch 'null 'vprint-symbol)

(defun vprint-string (output string)
  (with-color (cl-colors2:+tomato+ :stream output)
    (prin1 string output))
  (incf *position* (+ 2 (length string)))
  (values))

(set-vprint-dispatch 'string 'vprint-string)

(defun vprint-char (output char)
  (if (non-printable-char-p char)
      (let ((name (char-name char)))
        (with-color (cl-colors2:+limegreen+ :stream output)
          (format output "#\\~A" name))
        (incf *position* (+ 2 (length name))))
      (let ((representation (prin1-to-string char)))
        (with-color (cl-colors2:+limegreen+ :stream output)
          (princ representation output))
        (incf *position* (length representation))))
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
    (vprint (first form) output)
    (when (null (cdr form))
      (return-from vprint-macrocall (values)))
    (vprint-indent :block 3 output)
    (write-char #\Space output)
    (loop :repeat (count-pre-body-forms (millet:lambda-list (car form)))
          :for (elt . rest) :on (cdr form)
          :do (vprint-newline :fill output)
              (write elt :stream output)
              (when (null rest)
                (return-from vprint-macrocall (values)))
              (write-char #\Space output)
          :finally (vprint-indent :block 1 output)
                   (vprint-newline :fill output)
                   ;; body
                   (loop :for (elt . rest) :on rest
                         :do (write elt :stream output)
                             (when (null rest)
                               (return-from vprint-macrocall (values)))
                             (write-char #\Space output)
                             (vprint-newline :fill output)))))

(defun vprint-funcall (output form)
  (vprint-logical-block (output output :prefix "(" :suffix ")")
    (vprint (first form) output)
    (cond ((null (cdr form)) (return-from vprint-funcall (values)))
          ((atom (cdr form))
           (write-char #\Space output)
           (write-char #\. output)
           (write-char #\Space output)
           (incf *position* 3)
           (vprint (cdr form) output))
          (t
           (write-char #\Space output)
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
                         (write-char #\. output)
                         (write-char #\Space output)
                         (incf *position* 2)
                         (vprint list output))
                        ((consp list)
                         (vprint (car list) output)
                         (when (cdr list)
                           (write-char #\Space output)
                           (incf *position* 1)
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

;;;; VPRINT

(defun vprint (exp &optional output)
  (let ((*print-right-margin* (or *print-right-margin* +default-line-width+))
        (*position* 0)
        (*newlinep* *newlinep*))
    (vprint-logical-block (stream output)
      (funcall (vprint-dispatch exp) stream exp))))