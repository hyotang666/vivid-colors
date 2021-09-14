(in-package :cl-user)

#.(flet ((reexport-from (package symbols)
           `((:import-from ,package ,@symbols) (:export ,@symbols))))
    `(defpackage :vivid-colors
       (:use :cl)
       ,@(reexport-from :vivid-colors.content '(#:*print-vivid*))
       ,@(reexport-from :vivid-colors.dispatch '(#:*vprint-dispatch*
                                                  #:define-vprint-dispatch
                                                  #:in-vprint-dispatch
                                                  #:find-vprint-dispatch))
       ,@(reexport-from :vivid-colors.stream '(#:put #:put-strings
                                                     #:vprint-newline
                                                     #:vprint-indent
                                                     #:vprint-pop
                                                     #:vprint-exit-if-list-exhausted
                                                     #:vprint-logical-block
						     #:vprint))
       ))

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

;;;; PRINTERS

(defun default-printer (output exp) (put exp output) (values))

(setf vivid-colors.dispatch:*default-printer* 'default-printer)

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
    (vprint-newline :fill output)
    (vprint-indent :current 0 output)
    (loop (vprint (vprint-pop) output t)
          (vprint-exit-if-list-exhausted)
          (write-char #\Space output)
          (vprint-newline :fill output))))

(defun vprint-list (output list &optional (print-paren t) (newline-kind :fill))
  (cond
    ((and (symbolp (car list)) (macro-function (car list)) print-paren)
     (vprint-macrocall output list))
    ((and (symbolp (car list)) (fboundp (car list)) print-paren)
     (vprint-funcall output list))
    (t
     (vprint-logical-block (output (the list list) :prefix
                            (if print-paren
                                "("
                                "")
                            :suffix
                            (if print-paren
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
              :do (vprint-logical-block (output (the list bind) :prefix "("
                                         :suffix ")")
                    (vprint-exit-if-list-exhausted)
                    (vprint (vprint-pop) output t) ; var
                    (vprint-exit-if-list-exhausted)
                    (write-char #\Space output)
                    (vprint-newline :linear output)
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

(defun vprint-loop (output exp)
  (vprint-logical-block (output exp :prefix "(" :suffix ")")
    (vprint (vprint-pop) output t) ; operator.
    (vprint-exit-if-list-exhausted)
    (write-char #\Space output)
    (vprint-indent :current 0 output)
    (vprint-newline :miser output)
    (loop :for (elt . rest) :on (cdr exp)
          :do (vprint elt output t)
          :if (null rest)
            :do (loop-finish)
          :else :if (find (car rest)
                          '(for with collect collecting do doing sum summing
                            append appending nconc nconcing initially finally)
                          :test (lambda (x y) (and (symbolp x) (string= x y))))
            :do (vprint-newline :linear output)
          :else
            :do (write-char #\Space output)
                (vprint-newline :fill output))))

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
  (:set '(cons (member loop)) 'vprint-loop)
  (:set
   '(cons
      (member block unwind-protect prog1 return-from catch throw eval-when
              multiple-value-call multiple-value-prog1))
   'vprint-block))

(define-vprint-dispatch :standard
  (:merge :vivid :pretty))

(setq *vprint-dispatch* (find-vprint-dispatch :standard))
