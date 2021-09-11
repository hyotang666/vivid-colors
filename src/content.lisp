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

;;;; GF

(defgeneric compute-length (thing))

;;;; CHARACTER

(defmethod compute-length ((c character)) 1)

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

(defstruct object
  (content (error "CONTENT is required.") :type t :read-only t)
  (color nil :type list :read-only t)
  (key #'prin1-to-string :type function :read-only t))

(defmethod compute-length ((object object))
  (flet ((object-length (content)
           (let ((string? (funcall (object-key object) content)))
             (check-type string? string)
             (length string?))))
    (let* ((content (object-content object))
           (shared? (vivid-colors.shared:storedp content)))
      (cond ((not shared?) (object-length content))
            ((vivid-colors.shared:only-once-p content) (object-length content))
            ((vivid-colors.shared:already-printed-p content)
             (+ 2 ; For ##
                (length
                  (write-to-string (vivid-colors.shared:id shared?)
                                   :base 10))))
            (t
             (vivid-colors.shared:mark-printed content)
             (+ 2 ; For #=
                (length
                  (write-to-string (vivid-colors.shared:id shared?) :base 10))
                (object-length content)))))))

;;; COLORED-STRING

(defstruct colored-string (spec nil :type list :read-only t))

(defmacro dospec ((var <colored-string> &optional <return>) &body body)
  `(dolist (,var (colored-string-spec ,<colored-string>) ,<return>) ,@body))

(defmethod compute-length ((s colored-string))
  (let ((sum 2))
    (dospec (spec s sum)
      (etypecase spec
        (string (incf sum (length spec)))
        ((cons string) (incf sum (length (car spec))))))))

;;; SECTION

(defstruct (section (:conc-name nil))
  ;; Set by PRINCed.
  (start 0 :type (integer 0 #.most-positive-fixnum))
  (prefix "" :type simple-string :read-only t)
  (contents (vivid-colors.queue:new :type 'content)
            :type vivid-colors.queue:queue
            :read-only t)
  (suffix "" :type simple-string :read-only t))

(defun contents-list (section)
  (vivid-colors.queue:contents (contents section)))

(deftype content ()
  '(or object character indent newline section colored-string))

(defmacro docontents ((var <section> &optional <return>) &body body)
  `(vivid-colors.queue:for-each (,var (contents ,<section>) ,<return>)
     ,@body))

(defun add-content (object section) (setf (tail (contents section)) object))

(defmethod compute-length ((section section))
  (let ((sum 0))
    (declare (type (mod #.most-positive-fixnum) sum))
    (incf sum (length (prefix section)))
    (docontents (content section)
      (incf sum (compute-length content)))
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
