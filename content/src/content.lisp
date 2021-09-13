(in-package :cl-user)

(defpackage :vivid-colors.content
  (:use :cl)
  (:export ;;;; Constructors.
           #:make-section
           #:make-object
           #:make-colored-string
           #:make-indent
           #:make-newline)
  (:export #:add-content ; modifier.
           #:write-content ; Printer.
           #:newline-kind ; type.
           #:section ; type.
           ))

(in-package :vivid-colors.content)

(declaim (optimize speed))

;;;; SPECIAL VARIABLES

(declaim
 (type (or null (integer 0 #.most-positive-fixnum)) *position* *indent*))

(defvar *position*)

(defvar *indent*)

(declaim (type boolean *newlinep*))

(defvar *newlinep*)

(defparameter *print-vivid* t)

;;;; UTILITIES

(defmacro with-enclose ((<stream> <open> <close>) &body body)
  (let ((s (gensym "STREAM")) (o (gensym "OPEN")) (c (gensym "CLOSE")))
    `(let ((,s ,<stream>) (,o ,<open>) (,c ,<close>))
       (write-string ,o ,s)
       (incf *position* (length ,o))
       ,@body
       (write-string ,c ,s)
       (incf *position* (length ,c)))))

;;;; GF

(defgeneric compute-length (thing))

;; We do not want to overwrite PRINT-OBJECT for builtin type (i.e. character).

(defgeneric print-content (content output))

;;;; CHARACTER

(defmethod compute-length ((c character)) 1)

(defmethod print-content ((c character) (output stream))
  (write-char c output)
  (incf *position*)
  c)

;;;; INDENT

(deftype indent-kind () '(member :block :current))

(defstruct indent
  (kind :block :type indent-kind :read-only t)
  (width 0 :type (unsigned-byte 8) :read-only t))

(defmethod compute-length ((i indent)) 0)

;;;; NEWLINE
;;;; TYPES

(deftype newline-kind () '(member :mandatory :miser :fill :linear))

(defstruct newline
  (kind (error "KIND is required.") :type newline-kind :read-only t))

(defmethod compute-length ((n newline)) 0)

;;; OBJECT

(defun validate-color-spec (color)
  (destructuring-bind
      (color &key effect style)
      color
    (locally ; Out of responds.
     (declare (optimize (speed 1)))
     (check-type color cl-ansi-text:color-specifier))
    (and effect (assert (cl-ansi-text::find-effect-code effect)))
    (and style (assert (cl-ansi-text::find-style-code style))))
  color)

(defstruct (object (:constructor make-object
                    (&key content color key &aux
                     (color
                      (etypecase color
                        (null color)
                        (cons (validate-color-spec color)))))))
  (content (error "CONTENT is required.") :type t :read-only t)
  (color nil :type list :read-only t)
  (key #'prin1-to-string :type function :read-only t))

(defmethod compute-length ((object object))
  (flet ((object-length (content)
           (let ((string? (funcall (object-key object) content)))
             (check-type string? string)
             (length string?))))
    (declare
      (ftype (function (t) (values (mod #.array-total-size-limit) &optional))
             object-length))
    (let* ((content (object-content object))
           (shared? (and *print-circle* (vivid-colors.shared:storedp content))))
      (cond ((not shared?) (object-length content))
            ((vivid-colors.shared:only-once-p content) (object-length content))
            ((vivid-colors.shared:already-printed-p content)
             (+ 2 ; For ##
                (length
                  (write-to-string (vivid-colors.shared:id shared?)
                                   :base 10))))
            (t
             (vivid-colors.shared:mark-printed content)
             ;; I do not know how to fix note below.
             ;; note: unable to open-code float conversion in mixed numric operation
             ;; due to type uncertainty:
             ;; The second argument is a NUMBER, not a FLOAT.
             (locally
              (declare (optimize (speed 1)))
              (+ 2 ; For #=
                 (length
                   (write-to-string (vivid-colors.shared:id shared?) :base 10))
                 (object-length content))))))))

(defmethod print-content ((o object) (output stream))
  (let ((notation (funcall (object-key o) (object-content o))))
    (declare (type simple-string notation))
    (labels ((print-colored ()
               (write-string
                 (let ((cl-ansi-text:*color-mode* :8bit))
                   (apply #'cl-ansi-text:make-color-string (object-color o)))
                 output)
               (print-it)
               (write-string cl-ansi-text:+reset-color-string+ output))
             (print-refer (shared)
               (write-char #\# output)
               (write (vivid-colors.shared:id shared) :stream output :base 10)
               (write-char #\# output))
             (print-shared (shared printer)
               (write-char #\# output)
               (write (vivid-colors.shared:id shared) :stream output :base 10)
               (write-char #\= output)
               (funcall printer))
             (print-it ()
               (write-string notation output)))
      (if (object-color o)
          (if (not *print-circle*)
              (if *print-vivid*
                  (print-colored)
                  (print-it))
              (let ((shared? (vivid-colors.shared:storedp (object-content o))))
                (if (vivid-colors.shared:only-once-p (object-content o))
                    (if *print-vivid*
                        (print-colored)
                        (print-it))
                    (if (vivid-colors.shared:already-printed-p
                          (object-content o))
                        (print-refer shared?)
                        (print-shared shared?
                                      (if *print-vivid*
                                          #'print-colored
                                          #'print-it))))))
          (if (not *print-circle*)
              (print-it)
              (let ((shared? (vivid-colors.shared:storedp (object-content o))))
                (if (vivid-colors.shared:only-once-p (object-content o))
                    (print-it)
                    (if (vivid-colors.shared:already-printed-p
                          (object-content o))
                        (print-refer shared?)
                        (print-shared shared? #'print-it)))))))
    (incf *position* (length notation)))
  o)

;;; COLORED-STRING

(defstruct (colored-string (:constructor make-colored-string
                            (&key spec &aux
                             (spec
                              (mapc
                                (lambda (x)
                                  ;; Due to the cl-ansi-text:color-specifier.
                                  (declare (optimize (speed 1)))
                                  (etypecase x
                                    (string x)
                                    (cl-ansi-text:color-specifier x)
                                    (cons
                                     (check-type (car x) string)
                                     (validate-color-spec (cdr x)))))
                                spec)))))
  (spec nil :type list :read-only t))

(defmacro dospec ((var <colored-string> &optional <return>) &body body)
  `(dolist (,var (colored-string-spec ,<colored-string>) ,<return>) ,@body))

(defmethod compute-length ((s colored-string))
  (let ((sum 2))
    (declare (type (mod #.array-total-size-limit) sum))
    (dospec (spec s sum)
      (etypecase spec
        (string (incf sum (length spec)))
        ((cons string) (incf sum (length (the simple-string (car spec)))))))))

(defmethod print-content ((c colored-string) (output stream))
  (with-enclose (output "\"" "\"")
    (dospec (spec c)
      ;; Out of our responds. Etypecase emits many notes.
      (declare (optimize (speed 1)))
      (etypecase spec
        (string (write-string spec output) (incf *position* (length spec)))
        ((cons string (cons cl-ansi-text:color-specifier))
         (destructuring-bind
             (string . color)
             spec
           (when *print-vivid*
             (write-string (apply #'cl-ansi-text:make-color-string color)
                           output))
           (write-string string output)
           (when *print-vivid*
             (write-string cl-ansi-text:+reset-color-string+ output))
           (incf *position* (length string)))))))
  c)

;;; SECTION

(defstruct (section (:conc-name nil))
  ;; Set by PRINCed.
  (start 0 :type (integer 0 #.most-positive-fixnum))
  (prefix "" :type simple-string :read-only t)
  (contents (vivid-colors.queue:new :type 'content)
            :type vivid-colors.queue:queue
            :read-only t)
  (suffix "" :type simple-string :read-only t))

(defun contents-list (section) (vivid-colors.queue:contents (contents section)))

(deftype content ()
  '(or object character indent newline section colored-string))

(defmacro docontents ((var <section> &optional <return>) &body body)
  `(vivid-colors.queue:for-each (,var (contents ,<section>) ,<return>)
     ,@body))

(declaim
 (ftype (function (content section) (values content &optional)) add-content))

(defun add-content (object section)
  (setf (vivid-colors.queue:tail (contents section)) object))

(defmethod compute-length ((section section))
  (let ((sum 0))
    (declare (type (mod #.most-positive-fixnum) sum))
    (incf sum (length (prefix section)))
    (docontents (content section)
      (incf sum (the (mod #.array-total-size-limit) (compute-length content))))
    (incf sum (length (suffix section)))
    sum))

(defun mandatory? (section)
  (labels ((rec (s)
             (docontents (content s)
               (typecase content
                 (newline
                  (when (eq :mandatory (newline-kind content))
                    (return-from mandatory? t)))
                 (section (rec content))))))
    (rec section)))

(defun over-right-margin-p (contents)
  (and *print-right-margin*
       (<= (the fixnum *print-right-margin*)
           (the (mod #.array-total-size-limit)
                (reduce #'+ contents
                        :key #'compute-length
                        :initial-value *position*)))))

(defmethod print-content ((s section) (o stream))
  (setf (start s) *position*)
  (let ((*indent* (+ (start s) (length (prefix s)))))
    (labels ((miserp (rest)
               (and *print-miser-width*
                    *print-right-margin*
                    (<=
                      (-
                        (the (mod #.array-total-size-limit)
                             *print-right-margin*)
                        (start s))
                      (the (mod #.array-total-size-limit) *print-miser-width*))
                    (over-right-margin-p rest)))
             (newline (newlinep)
               (and newlinep (setf *newlinep* newlinep))
               (terpri o)
               (dotimes (x (setf *position* *indent*)) (write-char #\Space o)))
             (indent (indent)
               (setf *indent*
                       (mcase:emcase indent-kind (indent-kind indent)
                         (:block
                           *indent*
                           (+ (start s) (length (prefix s))
                              (indent-width indent)))
                         (:current
                           *indent*
                           (+ *position* (indent-width indent)))))))
      (cond
        ((or (not *print-pretty*)
             (and (not *newlinep*)
                  (not (mandatory? s))
                  (or (not *print-right-margin*)
                      (<=
                        (the (mod #.array-total-size-limit) (compute-length s))
                        (the fixnum *print-right-margin*)))))
         (with-enclose (o (prefix s) (suffix s))
           (docontents (content s)
             (typecase content
               ((or character object colored-string section)
                (print-content content o))))))
        (t
         (with-enclose (o (prefix s) (suffix s))
           (loop :for (content . rest) :on (contents-list s)
                 :do (etypecase content
                       (object (print-content content o))
                       (section (print-content content o))
                       (character (print-content content o))
                       (colored-string (print-content content o))
                       (newline
                        (let ((kind (newline-kind content)))
                          (mcase:emcase newline-kind kind
                            ((:mandatory :linear) (newline t))
                            (:miser
                              (when (miserp rest)
                                (newline t)))
                            (:fill
                              (when (or (over-right-margin-p rest) *newlinep*)
                                (newline nil))))))
                       (indent (indent content))))))))))

;;;; WRITE-CONTENT
;;; Interface for end user.
;;; We do not want to make end user care to wrap PRINT-CONTENT with WITH-PRINT-CONTEXT.

(defmacro with-print-context
          ((&key
            (vivid '*print-vivid*)
            (circle '*print-circle*)
            (pretty '*print-pretty*)
            (right-margin '*print-right-margin*)
            (miser-width '*print-miser-width*))
           &body body)
  `(let ((*print-vivid* ,vivid)
         (*print-circle* ,circle)
         (*print-pretty* ,pretty)
         (*print-right-margin* ,right-margin)
         (*print-miser-width* ,miser-width)
         (*position* 0)
         (*indent* 0)
         (*newlinep* nil))
     (vivid-colors.shared:with-check-object-seen () ,@body)))

(set-pprint-dispatch '(cons (member with-print-context))
                     (formatter
                      #.(concatenate 'string "~:<" ; pprint-logical-block.
                                     "~W~^~1I ~@_" ; operator.
                                     (concatenate 'string "~:<" ; lambda-list
                                                  "~@{~W~^ ~@_~W~^ ~_~}"
                                                  "~:>~^ ~_")
                                     "~@{~W~^ ~_~}" "~:>")))

(declaim
 (ftype (function
         (content &key (:stream stream) (:vivid boolean) (:circle boolean)
          (:pretty boolean)
          (:right-margin (or null (mod #.array-total-size-limit)))
          (:miser-width (or null (mod #.array-total-size-limit))))
         (values t &optional))
        write-content))

(defun write-content
       (content
        &key (stream *standard-output*) ((:vivid *print-vivid*) *print-vivid*)
        ((:circle *print-circle*) *print-circle*)
        ((:pretty *print-pretty*) *print-pretty*)
        ((:right-margin *print-right-margin*) *print-right-margin*)
        ((:miser-width *print-miser-width*) *print-miser-width*))
  (with-print-context () (print-content content stream)))