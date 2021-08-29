(in-package :cl-user)

(defpackage :vivid-colors
  (:use :cl)
  (:export))

(in-package :vivid-colors)

;;;; UTILITIES

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

;;;; CONFIGURATIONS

(defconstant +default-line-width+ 80)

(defparameter *vprint-dispatch* nil)

(declaim (type indent *position*))

(defvar *position*)

(defmethod documentation ((o (eql '*position*)) (type (eql 'variable)))
  "Current cursor position of the vstream.")

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

(defun default-printer (output exp) (write exp :stream output))

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
               :documentation "Storing lines and newline-kinds.")
   (queue-tail :reader tail)
   (prefix :initform "" :initarg :prefix :reader prefix)
   (suffix :initform "" :initarg :suffix :reader suffix)))

(defmethod initialize-instance :after ((o vprint-stream) &rest args)
  (declare (ignore args))
  (setf (indent o) (length (prefix o))
        (slot-value o 'queue-tail) (head o)))

(defun (setf tail) (new vstream)
  (rplacd (tail vstream) (setf (slot-value vstream 'queue-tail) (list new)))
  new)

(defmethod trivial-gray-streams:stream-write-char
           ((s vprint-stream) (c character))
  (vector-push-extend c (buffer s))
  (incf *position*)
  c)

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

;;;; PRINTERS

(defun vprint-keyword (output keyword)
  (with-color (cl-colors2:+yellow+ :stream output)
    (prin1 keyword output))
  (values))

(set-vprint-dispatch 'keyword 'vprint-keyword)

(defun vprint-real (output real)
  (let ((representation (prin1-to-string real)))
    (with-color (cl-colors2:+violet+ :stream output)
      (princ representation output))
    (values)))

(set-vprint-dispatch 'real 'vprint-real)

(defun vprint-symbol (output symbol) (prin1 symbol output) (values))

(set-vprint-dispatch '(and symbol (not keyword)) 'vprint-symbol)

(defun vprint-string (output string)
  (with-color (cl-colors2:+tomato+ :stream output)
    (prin1 string output))
  (values))

(set-vprint-dispatch 'string 'vprint-string)

(defun vprint-char (output char)
  (if (non-printable-char-p char)
      (let ((name (char-name char)))
        (with-color (cl-colors2:+limegreen+ :stream output)
          (format output "#\\~A" name)))
      (let ((representation (prin1-to-string char)))
        (with-color (cl-colors2:+limegreen+ :stream output)
          (princ representation output))))
  (values))

(set-vprint-dispatch 'character 'vprint-char)
