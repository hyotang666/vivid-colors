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

;;;; CONFIGURATIONS

(defconstant +default-line-width+ 80)

(defvar *vstream*)

(defparameter *print-vivid* t)

;;;; VPRINT-STREAM

(defclass vprint-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((output :type stream
           :initarg :output
           :reader output
           :initform *standard-output*
           :documentation "Underlying actual stream.")
   (section :initarg :section
            :type section
            :accessor section
            :documentation "Section block.")))

(defmethod initialize-instance :after
           ((o vprint-stream) &key (prefix "") (suffix "") &allow-other-keys)
  (setf (section o) (make-section :prefix prefix :suffix suffix)))

#+(or ccl clisp)
(defmethod trivial-gray-streams:stream-line-column ((s vprint-stream)) nil)

;; Adding character.

(defmethod trivial-gray-streams:stream-write-char
           ((s vprint-stream) (c character))
  (add-content c (section s)))

;; Adding object.

(defun color-spec-p (spec)
  (or (null spec)
      (typep spec 'cl-ansi-text:color-specifier)
      (when (listp spec)
        (destructuring-bind
            (color &key effect style)
            spec
          (and (typep color 'cl-ansi-text:color-specifier)
               (or (null effect) (cl-ansi-text::find-effect-code effect))
               (or (null style) (cl-ansi-text::find-style-code style)))))))

(declaim
 (ftype (function
         (t vprint-stream &key (:color (satisfies color-spec-p))
          (:key (or symbol function)))
         (values t &optional))
        put))

(defun put
       (content output
        &key color (key #'prin1-to-string)
        &aux (key (coerce key 'function)))
  (add-content
    (make-object :content content :color (uiop:ensure-list color) :key key)
    (section output))
  content)

(declaim
 (ftype (function (list vprint-stream) (values null &optional)) put-strings))

(defun put-strings (strings output)
  (assert (every
            (lambda (x)
              (typep x '(or string (cons string (satisfies color-spec-p)))))
            strings))
  (add-content (make-colored-string :spec strings) (section output))
  nil)

(declaim
 (ftype (function ((member :block :current) (unsigned-byte 8) vprint-stream)
         (values))
        vprint-indent))

(defun vprint-indent (kind width output)
  (add-content (make-indent :kind kind :width width) (section output))
  (values))

(declaim
 (ftype (function (newline-kind vprint-stream) (values)) vprint-newline))

(defun vprint-newline (kind output)
  #+clisp
  (progn (check-type kind newline-kind) (check-type output vprint-stream))
  (add-content (make-newline :kind kind) (section output))
  (values))

;;;; FINISH-OUTPUT as actual output.

(declaim (type (integer 0 #.most-positive-fixnum) *position* *indent*))

(defvar *position*)

(defvar *indent*)

(declaim (type boolean *newlinep*))

(defvar *newlinep* nil)

(defgeneric print-content (content output))

(defmethod print-content ((c character) (s vprint-stream))
  (write-char c (output s))
  (incf *position*)
  c)

(defmethod print-content ((o object) (s vprint-stream))
  (let ((notation (funcall (object-key o) (object-content o))))
    (labels ((print-colored ()
               (write-string
                 (let ((cl-ansi-text:*color-mode* :8bit))
                   (apply #'cl-ansi-text:make-color-string (object-color o)))
                 (output s))
               (print-it)
               (write-string cl-ansi-text:+reset-color-string+ (output s)))
             (print-refer (shared)
               (write-char #\# (output s))
               (write (vivid-colors.shared:id shared)
                      :stream (output s)
                      :base 10)
               (write-char #\# (output s)))
             (print-shared (shared printer)
               (write-char #\# (output s))
               (write (vivid-colors.shared:id shared)
                      :stream (output s)
                      :base 10)
               (write-char #\= (output s))
               (funcall printer))
             (print-it ()
               (write-string notation (output s))))
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

(defmethod print-content ((c colored-string) (o vprint-stream))
  (write-char #\" (output o))
  (incf *position*)
  (dospec (spec c)
    (etypecase spec
      (string (write-string spec (output o)) (incf *position* (length spec)))
      ((cons string (cons cl-ansi-text:color-specifier))
       (when *print-vivid*
         (write-string (apply #'cl-ansi-text:make-color-string (cdr spec))
                       (output o)))
       (write-string (car spec) (output o))
       (when *print-vivid*
         (write-string cl-ansi-text:+reset-color-string+ (output o)))
       (incf *position* (length (car spec))))))
  (write-char #\" (output o))
  (incf *position*)
  c)

(defmacro with-enclose ((<section> <stream>) &body body)
  (let ((s (gensym "SECTION")) (v (gensym "VPRINT-STREAM")))
    `(let ((,s ,<section>) (,v ,<stream>))
       (write-string (prefix ,s) (output ,v))
       (incf *position* (length (prefix ,s)))
       ,@body
       (write-string (suffix ,s) (output ,v))
       (incf *position* (length (suffix ,s))))))

(defmethod print-content ((s section) (o vprint-stream))
  (setf (start s) *position*)
  (let ((*indent* (+ (start s) (length (prefix s)))))
    (labels ((over-right-margin-p (rest)
               (and *print-right-margin*
                    (<= *print-right-margin*
                        (reduce #'+ rest
                                :key #'compute-length
                                :initial-value *position*))))
             (miserp (rest)
               (and *print-miser-width*
                    *print-right-margin*
                    (<= (- *print-right-margin* (start s)) *print-miser-width*)
                    (over-right-margin-p rest)))
             (newline (newlinep)
               (and newlinep (setf *newlinep* newlinep))
               (terpri (output o))
               (dotimes (x *indent*) (write-char #\Space (output o))))
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
             (null *print-right-margin*)
             (and (not *newlinep*)
                  (<= (compute-length s) *print-right-margin*)
                  (not (mandatory? s))))
         (with-enclose (s o)
           (docontents (content s)
             (typecase content
               ((or character object colored-string section)
                (print-content content o))))))
        (t
         (with-enclose (s o)
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

(defmethod trivial-gray-streams:stream-finish-output ((s vprint-stream))
  (setq st s)
  (vivid-colors.shared:with-check-object-seen ()
    (let ((*position* 0) (*newlinep* nil))
      (vivid-colors.shared:context () (print-content (section s) s)))))

;;;; DSL

(eval-when (:compile-toplevel)
  ;; VPRINT-LOGICAL-BLOCK needs this eval-when.
  (defun <vlb-body> (<list> ?list ?stream ?block body)
    (labels ((<whole> ()
               `(if (not (listp ,?list))
                    (vprint ,?list ,?stream t)
                    ,(<body>)))
             (<body> ()
               `(macrolet ((vprint-pop ()
                             `(if (consp ,',?list)
                                  (prog1
                                      (vivid-colors.shared:store
                                        (car ,',?list))
                                    (setf ,',?list
                                            (vivid-colors.shared:store
                                              (cdr ,',?list))))
                                  (prog1 (vivid-colors.shared:store ,',?list)
                                    (write-char #\. ,',?stream)
                                    (write-char #\Space ,',?stream)
                                    (setf ,',?list nil))))
                           (vprint-exit-if-list-exhausted ()
                             `(unless ,',?list
                                (return-from ,',?block (values)))))
                  ,@body)))
      (cond ((typep <list> '(cons (eql the) (cons (eql list)))) (<body>))
            ((not (constantp <list>)) (<whole>))
            ((listp (eval <list>)) (<body>))
            (t `(vprint ,?list ,?stream t))))))

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
       (vivid-colors.shared:context ()
         (block ,b
           (unwind-protect ,(<vlb-body> <list> l var b body)
             (if ,o
                 (finish-output ,var)
                 (progn
                  (add-content (section *vstream*) ,s)
                  (setf (section *vstream*) ,s)))))))))

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
    (write-char #\Space output)
    (loop :repeat (count-pre-body-forms (millet:lambda-list (car form)))
          :for elt := (vprint-pop)
          :do (vprint-newline :miser output)
              (vprint elt output t)
              (vprint-exit-if-list-exhausted)
              (write-char #\Space output)
          :finally (vprint-indent :block 1 output)
                   (vprint-newline :linear output)
                   ;; body
                   (loop :for elt := (vprint-pop)
                         :do (vprint elt output t)
                             (vprint-exit-if-list-exhausted)
                             (write-char #\Space output)
                             (vprint-newline :linear output)))))

(defun vprint-funcall (output form)
  (vprint-logical-block (output form :prefix "(" :suffix ")")
    (vprint (vprint-pop) output t)
    (vprint-exit-if-list-exhausted)
    (write-char #\Space output)
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
             (write-char #\Space output)
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
              (progn (write-char #\Space output) (vprint-newline :fill output))
              (return))))))

(defun vprint-array (output array)
  (labels ((rec (dims indices output)
             (if (endp dims)
                 (vprint (apply #'aref array (reverse indices)) output)
                 (vprint-logical-block (output nil :prefix "(" :suffix ")")
                   (dotimes (x (car dims))
                     (rec (cdr dims) (cons x indices) output)
                     (when (< (1+ x) (car dims))
                       (write-char #\Space output)))))))
    (vprint-logical-block (output nil
                                  :prefix (format nil "#~DA"
                                                  (array-rank array)))
      (rec (array-dimensions array) nil output))))

(defun vprint-structure (output structure)
  (vprint-logical-block (output nil :prefix "#S(" :suffix ")")
    (vprint (type-of structure) output)
    (write-char #\Space output)
    (vprint-indent :current 0 output)
    (loop :for (slot . rest) :on (c2mop:class-slots (class-of structure))
          :for name = (c2mop:slot-definition-name slot)
          :do (vprint (intern (symbol-name name) :keyword) output t)
              (write-char #\Space output)
              (vprint (slot-value structure name) output t)
              (when rest
                (write-char #\Space output)
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
    (write-char #\Space output)
    (vprint-indent :current 0 output)
    (vprint-newline :miser output)
    ;; Binds.
    (vprint-logical-block (output (vprint-pop) :prefix "(" :suffix ")")
      (vprint-exit-if-list-exhausted)
      (loop :for bind = (vprint-pop)
            :if (atom bind)
              :do (vprint bind output t)
                  (vprint-exit-if-list-exhausted)
                  (write-char #\Space output)
            :else
              :do (vprint-logical-block (output (the list bind)
                                                :prefix "("
                                                :suffix ")")
                    (vprint-exit-if-list-exhausted)
                    (vprint (vprint-pop) output t) ; var
                    (vprint-exit-if-list-exhausted)
                    (write-char #\Space output)
                    (vprint-newline :miser output)
                    (loop (vprint (vprint-pop) output t) ; form
                          (vprint-exit-if-list-exhausted)
                          (write-char #\Space output)
                          (vprint-newline :linear output)))
                  (vprint-exit-if-list-exhausted)
                  (write-char #\Space output)))
    ;; Body
    (vprint-exit-if-list-exhausted)
    (write-char #\Space output)
    (vprint-indent :block 1 output)
    (vprint-newline :linear output)
    (loop (vprint (vprint-pop) output t)
          (vprint-exit-if-list-exhausted)
          (write-char #\Space output)
          (vprint-newline :linear output))))

(defun vprint-block (output exp)
  (vprint-logical-block (output exp :prefix "(" :suffix ")")
    (vprint (vprint-pop) output t) ; operator
    (vprint-exit-if-list-exhausted)
    (write-char #\Space output)
    (vprint-indent :block 1 output)
    (vprint-newline :miser output)
    (loop (vprint (vprint-pop) output t)
          (vprint-exit-if-list-exhausted)
          (write-char #\Space output)
          (vprint-newline :linear output))))

(defun vprint-if (output exp)
  (vprint-logical-block (output exp :prefix "(" :suffix ")")
    (vprint (vprint-pop) output t) ; operator
    (vprint-exit-if-list-exhausted)
    (write-char #\Space output)
    (vprint-indent :current 0 output)
    (loop (vprint (vprint-pop) output t)
          (vprint-exit-if-list-exhausted)
          (write-char #\Space output)
          (vprint-newline :linear output))))

(defun vprint-progn (output exp)
  (vprint-logical-block (output exp :prefix "(" :suffix ")")
    (vprint (vprint-pop) output t) ; operator
    (vprint-exit-if-list-exhausted)
    (write-char #\Space output)
    (vprint-indent :block 1 output)
    (vprint-newline :linear output)
    (loop (vprint (vprint-pop) output t)
          (vprint-exit-if-list-exhausted)
          (write-char #\Space output)
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
            (*print-miser-width* (or *print-miser-width* 60)))
        (vprint-logical-block (output nil)
          (funcall (coerce (vprint-dispatch exp) 'function) output exp)))))