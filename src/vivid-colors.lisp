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

;;;; CONFIGURATIONS

(defconstant +default-line-width+ 80)

(defparameter *vprint-dispatch* nil)

(declaim (type boolean *newlinep*))

(defvar *newlinep* nil)

(defvar *vstream*)

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

(defun compute-length (thing)
  (etypecase thing
    (line (line-length thing))
    (section
     (let ((sum 0))
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
    (incf (view-length output) (length representation)))
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
  (vector-push-extend c (buffer s))
  c)

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
         (unwind-protect (progn ,@body (values))
           (when cl-ansi-text:*enabled*
             (princ cl-ansi-text:+reset-color-string+ ,output)))))))

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

;;;; PRINTERS

(defun vprint-keyword (output keyword)
  (with-color (cl-colors2:+yellow+ :stream output)
    (prin1 keyword output))
  (incf (view-length output) (1+ (length (symbol-name keyword))))
  (values))

(set-vprint-dispatch 'keyword 'vprint-keyword)

(defun vprint-real (output real)
  (let ((representation (prin1-to-string real)))
    (with-color (cl-colors2:+violet+ :stream output)
      (write-string representation output))
    (incf (view-length output) (length representation))
    (values)))

(set-vprint-dispatch 'real 'vprint-real)

(defun vprint-symbol (output symbol)
  (let ((representation (prin1-to-string symbol)))
    (write-string representation output)
    (incf (view-length output) (length representation)))
  (values))

(set-vprint-dispatch '(and symbol (not keyword)) 'vprint-symbol)

(set-vprint-dispatch 'null 'vprint-symbol)

(defun vprint-string (output string)
  (with-color (cl-colors2:+tomato+ :stream output)
    (prin1 string output))
  (incf (view-length output) (+ 2 (length string)))
  (values))

(set-vprint-dispatch 'string 'vprint-string)

(defun vprint-char (output char)
  (if (non-printable-char-p char)
      (let ((name (char-name char)))
        (with-color (cl-colors2:+limegreen+ :stream output)
          (format output "#\\~A" name))
        (incf (view-length output) (+ 2 (length name))))
      (let ((representation (prin1-to-string char)))
        (with-color (cl-colors2:+limegreen+ :stream output)
          (princ representation output))
        (incf (view-length output) (length representation))))
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
    (%vprint (first form) output)
    (when (null (cdr form))
      (return-from vprint-macrocall (values)))
    (vprint-indent :block 3 output)
    (write-char #\Space output)
    (incf (view-length output))
    (loop :repeat (count-pre-body-forms (millet:lambda-list (car form)))
          :for (elt . rest) :on (cdr form)
          :do (vprint-newline :miser output)
              (%vprint elt output)
              (when (null rest)
                (return-from vprint-macrocall (values)))
              (write-char #\Space output)
              (incf (view-length output))
          :finally (vprint-indent :block 1 output)
                   (vprint-newline :fill output)
                   ;; body
                   (loop :for (elt . rest) :on rest
                         :do (%vprint elt output)
                             (when (null rest)
                               (return-from vprint-macrocall (values)))
                             (write-char #\Space output)
                             (incf (view-length output))
                             (vprint-newline :fill output)))))

(defun vprint-funcall (output form)
  (vprint-logical-block (output output :prefix "(" :suffix ")")
    (%vprint (first form) output)
    (cond ((null (cdr form)) (return-from vprint-funcall (values)))
          ((atom (cdr form))
           (write-char #\Space output)
           (write-char #\. output)
           (write-char #\Space output)
           (incf (view-length output) 3)
           (%vprint (cdr form) output))
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
                         (incf (view-length output) 2)
                         (%vprint list output))
                        ((consp list)
                         (%vprint (car list) output)
                         (when (cdr list)
                           (write-char #\Space output)
                           (incf (view-length output) 1)
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
    (let ((namestring (namestring pathname)))
      (with-color (cl-colors2:+tomato+ :stream output)
        (prin1 namestring output))
      (incf (view-length output) (+ 4 (length namestring)))))
  (values))

(set-vprint-dispatch 'pathname 'vprint-pathname)

(defun vprint-structure (output structure)
  (vprint-logical-block (output output :prefix "#S(" :suffix ")")
    (let ((name (prin1-to-string (type-of structure))))
      (with-color (cl-colors2:+limegreen+ :stream output)
        (princ name output))
      (incf (view-length output) (length name)))
    (write-char #\Space output)
    (incf (view-length output))
    (vprint-indent :current 0 output)
    (loop :for (slot . rest) :on (c2mop:class-slots (class-of structure))
          :for name = (c2mop:slot-definition-name slot)
          :do (let ((namestring (prin1-to-string name)))
                (with-color (cl-colors2:+yellow+ :stream output)
                  (write-char #\: output)
                  (write-string namestring output))
                (incf (view-length output) (1+ (length namestring))))
              (write-char #\Space output)
              (incf (view-length output))
              (%vprint (slot-value structure name) output)
              (when rest
                (write-char #\Space output)
                (incf (view-length output))
                (vprint-newline :linear output)))))

(set-vprint-dispatch 'structure-object 'vprint-structure)

;;;; VPRINT

(defun vprint (exp &optional output)
  (let ((*print-right-margin* (or *print-right-margin* +default-line-width+))
        (*newlinep*))
    (vprint-logical-block (output output)
      (%vprint exp output))))

(defun %vprint (exp &optional output)
  (funcall (vprint-dispatch exp) output exp))
