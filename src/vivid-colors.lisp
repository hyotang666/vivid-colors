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

(defvar *vstream*)

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
  (setf (section o)
          (vivid-colors.content:make-section :prefix prefix :suffix suffix)))

#+(or ccl clisp)
(defmethod trivial-gray-streams:stream-line-column ((s vprint-stream)) nil)

;; Adding character.

(defmethod trivial-gray-streams:stream-write-char
           ((s vprint-stream) (c character))
  (vivid-colors.content:add-content c (section s)))

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
  (vivid-colors.content:add-content
    (vivid-colors.content:make-object :content content
                                      :color (uiop:ensure-list color)
                                      :key key)
    (section output))
  content)

(declaim
 (ftype (function (list vprint-stream) (values null &optional)) put-strings))

(defun put-strings (strings output)
  (assert (every
            (lambda (x)
              (typep x '(or string (cons string (satisfies color-spec-p)))))
            strings))
  (vivid-colors.content:add-content
    (vivid-colors.content:make-colored-string :spec strings) (section output))
  nil)

(declaim
 (ftype (function ((member :block :current) (unsigned-byte 8) vprint-stream)
         (values))
        vprint-indent))

(defun vprint-indent (kind width output)
  (vivid-colors.content:add-content
    (vivid-colors.content:make-indent :kind kind :width width)
    (section output))
  (values))

(declaim
 (ftype (function (vivid-colors.content:newline-kind vprint-stream) (values))
        vprint-newline))

(defun vprint-newline (kind output)
  #+clisp
  (progn
   (check-type kind vivid-colors.content:newline-kind)
   (check-type output vprint-stream))
  (vivid-colors.content:add-content
    (vivid-colors.content:make-newline :kind kind) (section output))
  (values))

;;;; FINISH-OUTPUT as actual output.

(defmethod trivial-gray-streams:stream-finish-output ((s vprint-stream))
  (setq st s)
  (vivid-colors.shared:with-check-object-seen ()
    (vivid-colors.shared:context ()
      (vivid-colors.content:print-content (section s) (output s)))))

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
                              (vivid-colors.content:make-section)
                              (vivid-colors.content:make-section :prefix ,prefix
                                                                 :suffix ,suffix)))
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
                  (vivid-colors.content:add-content (section *vstream*) ,s)
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