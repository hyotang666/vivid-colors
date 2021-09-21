(in-package :cl-user)

(defpackage :vivid-colors.content
  (:use :cl)
  (:export ;;;; Constructors (i.e. appointments objects.)
           #:make-section
           #:make-reference
           #:make-object
           #:make-colored-string
           #:make-indent
           #:make-newline)
  (:export ;;;; Types.
           #:newline-kind
           #:section
           #:reference)
  (:export #:add-content ; modifier. (i.e. printing appointments)
           #:write-content ; Printer (i.e. fulfills)
           #:*color* ; configuration.
           #:expression ; reader.
           #:check-circularity)
  (:documentation "Provide the feature of the printing appointments and its fulfills."))

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

(defparameter *color* nil)

;;;; UTILITIES

(defmacro with-enclose ((<stream> <open> <close> &optional <color>) &body body)
  (let ((s (gensym "STREAM"))
        (o (gensym "OPEN"))
        (c (gensym "CLOSE"))
        (color (gensym "COLOR")))
    `(let ((,s ,<stream>) (,o ,<open>) (,c ,<close>) (,color ,<color>))
       (unwind-protect
           (progn
            (when (and *print-vivid* ,color)
              (let ((cl-ansi-text:*color-mode* :8bit))
                (write-string (apply #'cl-ansi-text:make-color-string ,color)
                              ,s)))
            (write-string ,o ,s)
            (incf *position* (length ,o))
            (let ((*color* ,color))
              ,@body)
            (when (and *print-vivid* ,color)
              (let ((cl-ansi-text:*color-mode* :8bit))
                (write-string (apply #'cl-ansi-text:make-color-string ,color)
                              ,s)))
            (write-string ,c ,s)
            (incf *position* (length ,c)))
         (when (and *print-vivid* ,color)
           (write-string cl-ansi-text:+reset-color-string+ ,s))))))

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

(defun check-circularity ()
  (unless *print-circle*
    (cerror "Asign *print-circle* with T."
            "Can not handle circular list due to *print-circle* := NIL.")
    (setf *print-circle* t)))

;;;; GF
;; NOTE: COMPUTE-LENGTH responds to initialize shared-id.

(defgeneric compute-length (thing))

(defun compute-shared-length (exp)
  (let ((shared (vivid-colors.shared:sharedp exp t)))
    (+ 2 ; ## or #=
       (length
         (write-to-string
           (vivid-colors.shared:id shared :if-does-not-exist :set)
           :base 10)))))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; ccl needs this eval-when. [1]
  ;; LOOP macro in the method PRINT-CONTENT for SECTION use :OF-TYPE declaration.
  ;; CCL needs the knowledge about the type CONTENT in compile time.
  (defstruct (indent #+clisp
                     (:constructor make-indent
                      (&key kind width &aux
                       (kind (progn (check-type kind indent-kind) kind))
                       (width
                        (progn (check-type width (unsigned-byte 8)) width)))))
    "An appointment of the indentation."
    (kind :block :type indent-kind :read-only t)
    (width 0 :type (unsigned-byte 8) :read-only t)))

(defmethod compute-length ((i indent)) 0)

;;;; NEWLINE
;;;; TYPES

(deftype newline-kind () '(member :mandatory :miser :fill :linear))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; [1]
  (defstruct (newline #+clisp
                      (:constructor make-newline
                       (&key kind &aux
                        (kind (progn (check-type kind newline-kind) kind)))))
    "An appointment of the pretty newline."
    (kind (error "KIND is required.") :type newline-kind :read-only t)))

(defmethod compute-length ((n newline)) 0)

;;; OBJECT

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; [1]
  (defstruct (object (:constructor make-object
                      (&key content color key &aux
                       (firstp (vivid-colors.shared:store content))
                       (color
                         (etypecase color
                           (null color)
                           (cons (validate-color-spec color))))
                       #+clisp (key (progn (check-type key function) key)))))
    "An appointment of printing the lisp object."
    ;; The lisp value.
    (content (error "CONTENT is required.") :type t :read-only t)
    ;; Does this content in the first appearance in the expression?
    (firstp t :type boolean :read-only t)
    ;; Printed color.
    (color nil :type list :read-only t)
    ;; Representation generator. e.g. string must have #\" around.
    (key #'prin1-to-string :type function :read-only t)))

(declaim
 (ftype (function (t) (values (mod #.array-total-size-limit) &optional))
        compute-shared-length))

(defmethod compute-length ((object object))
  (flet ((object-length (content)
           (let ((string? (funcall (object-key object) content)))
             (check-type string? string)
             (length string?))))
    (declare
      (ftype (function (*) (values (mod #.array-total-size-limit) &optional))
             object-length))
    (let* ((content (object-content object))
           (shared? (and *print-circle* (vivid-colors.shared:sharedp content))))
      (cond ((not shared?) (object-length content))
            ((not (object-firstp object)) (compute-shared-length content))
            (t
             (the (mod #.array-total-size-limit)
                  (+ (compute-shared-length content)
                     (object-length content))))))))

(defmethod print-content ((o object) (output stream))
  (let ((notation
         (let ((*print-circle*))
           (funcall (object-key o) (object-content o)))))
    (declare (type simple-string notation))
    (labels ((print-colored ()
               (write-string
                 (let ((cl-ansi-text:*color-mode* :8bit))
                   (apply #'cl-ansi-text:make-color-string
                          (or *color* (object-color o))))
                 output)
               (print-it)
               (write-string cl-ansi-text:+reset-color-string+ output))
             (print-refer (shared char)
               (write-char #\# output)
               (write (vivid-colors.shared:id shared :if-does-not-exist :error)
                      :stream output
                      :base 10)
               (write-char char output))
             (print-it ()
               (write-string notation output)))
      (or (let (shared?)
            (when (and *print-circle*
                       (setf shared?
                               (vivid-colors.shared:sharedp
                                 (object-content o))))
              (if (not (object-firstp o))
                  (print-refer shared? #\#)
                  (if (and *print-vivid* (or *color* (object-color o)))
                      (progn (print-refer shared? #\=) (print-colored))
                      (progn (print-refer shared? #\=) (print-it))))))
          (if (and *print-vivid* (or *color* (object-color o)))
              (print-colored)
              (print-it))))
    (incf *position* (length notation)))
  o)

;;; COLORED-STRING

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; [1]
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
    "An appointment of printing the partially colored string."
    (spec nil :type list :read-only t)))

;; An abstraction barriar as ITERATOR.

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; [1]
  (defstruct (section (:conc-name nil)
                      (:constructor make-section
                       (&key start prefix contents suffix color expression &aux
                        #+clisp
                        (start
                          (progn
                           (check-type start
                                       (integer 0 #.most-positive-fixnum))
                           start))
                        #+clisp
                        (prefix
                          (progn (check-type prefix simple-string) prefix))
                        #+clisp
                        (contents
                          (progn
                           (check-type contents vivid-colors.queue:queue)
                           contents))
                        #+clisp
                        (suffix
                          (progn (check-type suffix simple-string) suffix))
                        #+clisp
                        (color
                          (progn
                           (check-type color
                                       (or null
                                           (satisfies validate-color-spec)))
                           color))
                        (expression
                          (progn
                           (vivid-colors.shared:store expression)
                           expression)))))
    "An appointment of printing the section."
    ;; Set by PRINCed.
    (start 0 :type (integer 0 #.most-positive-fixnum))
    (prefix "" :type simple-string :read-only t)
    ;; Appointments to be printed.
    (contents (vivid-colors.queue:new :type 'content)
              :type vivid-colors.queue:queue
              :read-only t)
    ;; Actual lisp object.
    (expression nil :type t :read-only t)
    (suffix "" :type simple-string :read-only t)
    (color *color*
           :type (or null (satisfies validate-color-spec))
           :read-only t)))

;;; An abstraction barriar as ITERATOR.

(defmacro docontents ((var <section> &optional <return>) &body body)
  `(vivid-colors.queue:for-each (,var (contents ,<section>) ,<return>)
     ,@body))

(defun circular-reference-p (section)
  (labels ((rec (s)
             (docontents (content s)
               (typecase content
                 (reference (return t))
                 (section (rec content))))))
    (rec section)))

(defun count-content (section)
  (let ((count 0))
    (declare (type (mod #.array-total-size-limit) count))
    (docontents (content section count)
      (if (typep content '(or object section reference))
          (incf count)))))

(defmethod compute-length ((section section))
  (declare
   (ftype (function (*) (values (mod #.array-total-size-limit) &optional))
          compute-length))
  (let ((sum 0))
    (declare (type (mod #.most-positive-fixnum) sum))
    (labels ((do-circular ()
               (docontents ((content . rest) section)
                 (declare (type list rest))
                 (typecase content
                   (reference
                    (if (or (find-if #'contentp rest)
                            (= 1 (count-content section)))
                        (incf sum
                              (compute-shared-length
                                (expression (reference-section content))))
                        (incf sum
                              (+ 3 ; " . "
                                 (compute-shared-length
                                   (expression
                                     (reference-section content)))))))
                   (otherwise (incf sum (compute-length content))))))
             (contentp (thing)
               (typep thing '(or object section reference)))
             (body (fun)
               (incf sum (length (prefix section)))
               (funcall fun)
               (incf sum (length (suffix section)))))
      (cond
        ((circular-reference-p section)
         (when (alexandria:circular-list-p (expression section))
           (check-circularity))
         (incf sum (compute-shared-length (expression section)))
         (body #'do-circular))
        (*print-circle* (body #'do-circular))
        (t
         (body
           (lambda ()
             (docontents (content section)
               (incf sum (compute-length content))))))))
    sum))

(defun mandatory? (section)
  (let ((seen (make-hash-table :test #'eq)))
    (setf (gethash section seen) t)
    (labels ((rec (s)
               (docontents (content s)
                 (typecase content
                   (newline
                    (when (eq :mandatory (newline-kind content))
                      (return-from mandatory? t)))
                   (section
                    (unless (gethash content seen)
                      (setf (gethash content seen) t)
                      (rec content)))))))
      (rec section))))

(defun over-right-margin-p (contents)
  (and *print-right-margin*
       (<= (the fixnum *print-right-margin*)
           (the (mod #.array-total-size-limit)
                (reduce #'+ contents
                        :key #'compute-length
                        :initial-value *position*)))))

(defun miserp (rest section)
  (and *print-miser-width*
       *print-right-margin*
       (<=
         (- (the (mod #.array-total-size-limit) *print-right-margin*)
            (start section))
         (the (mod #.array-total-size-limit) *print-miser-width*))
       (over-right-margin-p rest)))

(defun indent (indent section)
  (setf *indent*
          (mcase:emcase indent-kind (indent-kind indent)
            (:block
              *indent*
              (+ (start section) (length (prefix section))
                 (indent-width indent)))
            (:current *indent* (+ *position* (indent-width indent))))))

(defun newline (newlinep section output)
  (and newlinep (setf *newlinep* t))
  (terpri output)
  (dotimes
      (x
       (setf *position*
               (if (eq :miser newlinep)
                   (setf *indent*
                           (+ (start section) (length (prefix section))))
                   *indent*)))
    (write-char #\Space output)))

(defmethod print-content ((s section) (o stream))
  (setf (start s) *position*)
  (let ((*indent* (+ (start s) (length (prefix s))))
        (length (compute-length s))) ; <--- Must do compute length first.
    (when (circular-reference-p s)
      (check-circularity)
      (format o "#~D="
              (vivid-colors.shared:id
                (vivid-colors.shared:sharedp (expression s) t)
                :if-does-not-exist :error)))
    (cond
      ((or (not *print-pretty*)
           (and (not *newlinep*)
                (not (mandatory? s))
                (or (not *print-right-margin*)
                    (<= (the (mod #.array-total-size-limit) length)
                        (the fixnum *print-right-margin*)))))
       (with-enclose (o (prefix s) (suffix s) (color s))
         (docontents ((content . rest) s)
           (declare (type list rest))
           (typecase content
             ((or character object colored-string section)
              (print-content content o))
             (reference
              (unless (or (find-if
                            (lambda (x)
                              (typep x '(or object section reference)))
                            rest)
                          (= 1 (count-content s)))
                (write-string " . " o))
              (print-content content o))))))
      (t
       (with-enclose (o (prefix s) (suffix s) (color s))
         (docontents ((content . rest) s)
           (declare (type list rest))
           (etypecase content
             (object (print-content content o))
             (section (print-content content o))
             (character (print-content content o))
             (colored-string (print-content content o))
             (reference (print-content content o))
             (newline
              (let ((kind (newline-kind content)))
                (mcase:emcase newline-kind kind
                  ((:mandatory :linear) (newline :linear s o))
                  (:miser
                    (when (miserp rest s)
                      (newline :miser s o)))
                  (:fill
                    (when (let ((next
                                 (find-if
                                   (lambda (x) (typep x '(or object section)))
                                   rest)))
                            (and next (over-right-margin-p (list next))))
                      (newline nil s o))))))
             (indent (indent content s)))))))))

;;;; REFERENCE

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; [1]
  (defstruct reference
    "The substitution of the circular reference."
    (section (error "SECTION is required.") :type section)))

(defmethod compute-length ((ref reference))
  (if (not *print-circle*)
      (compute-length (reference-section ref))
      (compute-shared-length (expression (reference-section ref)))))

(defmethod print-content ((ref reference) (o stream))
  (if (not *print-circle*)
      (print-content (reference-section ref) o)
      (format o "#~D#"
              (vivid-colors.shared:id
                (vivid-colors.shared:sharedp
                  (expression (reference-section ref)) t)
                :if-does-not-exist :error))))

(deftype content ()
  '(or object character indent newline section colored-string reference))

;;; An abstraction barriar as UPDATOR.

(declaim
 (ftype (function (content section) (values content &optional)) add-content))

(defun add-content (object section)
  (setf (vivid-colors.queue:tail (contents section)) object))

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
     ,@body))

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
  #+clisp
  (progn
   (check-type *print-right-margin* (or null unsigned-byte))
   (check-type *print-miser-width* (or null unsigned-byte)))
  (with-print-context () (print-content content stream)))
