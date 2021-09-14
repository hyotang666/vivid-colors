(in-package :cl-user)

(defpackage :vivid-colors.stream
  (:use :cl)
  (:export #:put
           #:put-strings
           #:vprint-indent
           #:vprint-newline
           #:vprint-pop
           #:vprint-exit-if-list-exhausted
           #:vprint-logical-block))

(in-package :vivid-colors.stream)

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

(defvar *vstream*)

;;;; VPRINT-STREAM

(defclass vprint-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((output :type stream
           :initarg :output
           :reader output
           :initform *standard-output*
           :documentation "Underlying actual stream.")
   (section :initarg :section
            :initform (vivid-colors.content:make-section)
            :type vivid-colors.content:section
            :accessor section
            :documentation "Section block.")))

#+(or ccl clisp)
(defmethod trivial-gray-streams:stream-line-column ((s vprint-stream)) nil)

;; Adding character.

(defmethod trivial-gray-streams:stream-write-char
           ((s vprint-stream) (c character))
  (vivid-colors.content:add-content c (section s)))

;; Adding object.

(declaim
 (ftype (function
         (t vprint-stream &key (:color (or cl-ansi-text:color-specifier cons))
          (:key (or symbol function)))
         (values t &optional))
        put))

(defun put
       (content output
        &key color (key #'prin1-to-string)
        &aux (key (coerce key 'function)))
  (vivid-colors.content:add-content
    (vivid-colors.content:make-object :content content
                                      :firstp (vivid-colors.shared:store
                                                content)
                                      :color (uiop:ensure-list color)
                                      :key key)
    (section output))
  content)

(declaim
 (ftype (function (list vprint-stream) (values null &optional)) put-strings))

(defun put-strings (strings output)
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
  (vivid-colors.content:write-content (section s) :stream (output s)))

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
                                  (prog1 (car ,',?list)
                                    (setf ,',?list
                                            (progn
                                             (vivid-colors.shared:store
                                               (cdr ,',?list))
                                             (cdr ,',?list))))
                                  (prog1 ,',?list
                                    (write-char #\. ,',?stream)
                                    (write-char #\Space ,',?stream)
                                    (setf ,',?list nil))))
                           (vprint-exit-if-list-exhausted ()
                             `(unless ,',?list
                                (return-from ,',?block (values)))))
                  (vivid-colors.shared:store ,?list)
                  ,@body)))
      (cond ((typep <list> '(cons (eql the) (cons (eql list)))) (<body>))
            ((not (constantp <list>)) (<whole>))
            ((listp (eval <list>)) (<body>))
            (t `(vprint ,?list ,?stream t)))))
  (defun <make-section> (<list> ?list ?prefix ?suffix)
    (symbol-macrolet ((<whole>
                       `(if (not (listp ,?list))
                            ,<then>
                            ,<else>))
                      (<then> `(vivid-colors.content:make-section))
                      (<else>
                       `(vivid-colors.content:make-section :prefix ,?prefix
                                                           :suffix ,?suffix)))
      (cond
        ((constantp <list>)
         (let ((value (eval <list>)))
           (if (not (listp value))
               <then>
               <else>)))
        (t <whole>))))
  (defun <xxxfix> (<list> ?list ?xxxfix)
    (symbol-macrolet ((<whole>
                       `(if (not (listp ,?list))
                            ,<then>
                            ,<else>))
                      (<then> "")
                      (<else> ?xxxfix))
      (cond
        ((constantp <list>)
         (let ((value (eval <list>)))
           (if (not (listp value))
               <then>
               <else>)))
        (t <whole>)))))

(defmacro vprint-logical-block
          ((<stream-var> <list> &key (prefix "") (suffix "")) &body body)
  (let ((s (gensym "SECTION"))
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
                          ,(<make-section> <list> l prefix suffix))
                  *vstream*)
                 (make-instance 'vprint-stream
                                :output ,var
                                :section (vivid-colors.content:make-section
                                           :prefix ,(<xxxfix> <list> l prefix)
                                           :suffix ,(<xxxfix> <list> l
                                                              suffix)))))
            (*vstream* ,var))
       (vivid-colors.shared:context ()
         (block ,b
           (unwind-protect ,(<vlb-body> <list> l var b body)
             (if (not ,s)
                 (finish-output ,var)
                 (progn
                  (vivid-colors.content:add-content (section *vstream*) ,s)
                  (setf (section *vstream*) ,s)))))))))

(defmacro vprint-pop () (error 'out-of-scope :name 'vprint-put))

(defmacro vprint-exit-if-list-exhausted ()
  (error 'out-of-scope :name 'vprint-exit-if-list-exhausted))
