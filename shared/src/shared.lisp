(in-package :cl-user)

(defpackage :vivid-colors.shared
  (:use :cl)
  (:shadow count)
  (:export #:context ; DSL.
           #:id ; Accessor.
           #:count ; Accessor.
           #:store ; Modifier.
           #:should-do-p ; Predicate.
           #:sharedp))

(in-package :vivid-colors.shared)

(declaim (optimize speed))

;;;; CONDITION

(define-condition without-context (cell-error program-error)
  ()
  (:report
   (lambda (this output)
     (funcall (formatter "Called without dynamic context of ~S.") output
              (cell-error-name this)))))

;;;; SHARED.
;; To support *print-circle*

(declaim (type (or null (integer 0 #.most-positive-fixnum)) *shared-counter*))

(defparameter *shared-counter* nil)

(declaim
 (type (or null hash-table) ; :TEST must be #'EQ.
  *shared-objects*))

(defparameter *shared-objects* nil)

(defmacro context (() &body body)
  `(let ((*shared-counter* (or *shared-counter* 0))
         (*shared-objects* (or *shared-objects* (make-hash-table :test #'eq))))
     ,@body))

(defstruct (shared (:predicate nil) (:conc-name nil))
  ;; Integer to identify object.
  ;; This is set when printing because we do not know actually printing it.
  ;; ID is used only the content satisfies SHOULD-DO-P and
  ;; reffered by two or more times and *print-circle* is true.
  (%id nil :type (or null (integer 0 #.most-positive-fixnum)))
  ;; How many times appear in the expression?
  (count 1 :type (integer 1 #.most-positive-fixnum)))

(defun id (shared &key (if-does-not-exist nil))
  (or (%id shared)
      (ecase if-does-not-exist
        ((nil))
        (:error (error "ID is not initialized yet. ~S" shared))
        (:set
         (if *shared-counter*
             (setf (%id shared) (incf *shared-counter*))
             (error 'without-context :name 'context))))))

(defun storedp (object)
  (if *shared-objects*
      (values (gethash object *shared-objects*))
      (error 'without-context :name 'context)))

(defun store (object)
  "Evaluated to be T only if actualy stored."
  (unless *shared-objects*
    (error 'without-context :name 'context))
  (when (should-do-p object)
    (let ((shared? (storedp object)))
      (if shared?
          (tagbody (incf (count shared?)))
          (progn (setf (gethash object *shared-objects*) (make-shared)) t)))))

(defun should-do-p (exp)
  "Should EXP be shared?"
  (not
    (or (and (symbolp exp) (symbol-package exp))
        (characterp exp)
        (numberp exp))))

(defun sharedp (exp &optional errorp)
  "Actually shared by two or more times?"
  (let ((shared? (storedp exp)))
    (or (when (and shared? (< 1 (count shared?)))
          shared?)
        (and errorp (error "Not ~:[stored.~;shared.~] ~S" shared? exp)))))

(defun pprint-context (output exp)
  (funcall
    (formatter
     #.(concatenate 'string "~:<" ; pprint-logical-block.
                    "~W~^~1I ~@_" ; operator.
                    (concatenate 'string "~:<" ; pprint-logical-block.
                                 "~@{~W~^ ~:_~}" "~:>~^ ~_")
                    "~@{~W~^ ~_~}" ; the body.
                    "~:>"))
    output exp))

(set-pprint-dispatch '(cons (member context)) 'pprint-context)