(in-package :cl-user)

(defpackage :vivid-colors.shared
  (:use :cl)
  (:shadow count)
  (:export #:context ; DSL.
           #:id ; Reader.
           #:count ; Accessor.
           #:store ; Modifier.
           #:storedp ; Predicate.
           #:should-do-p
           #:only-once-p
           #:with-check-object-seen
           #:already-printed-p
           #:mark-printed))

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

(declaim (type (or null (integer 0 #.most-positive-fixnum)) *shared-count*))

(defparameter *shared-count* nil)

(declaim
 (type (or null hash-table) ; :TEST must be #'EQ.
  *shared-objects*))

(defparameter *shared-objects* nil)

(defmacro context (() &body body)
  `(let ((*shared-count* (or *shared-count* 0))
         (*shared-objects* (or *shared-objects* (make-hash-table :test #'eq))))
     ,@body))

(defstruct (shared (:predicate nil) (:conc-name nil))
  (id (error "ID is required.")
      :type (integer 0 #.most-positive-fixnum)
      :read-only t)
  (count 0 :type (integer 0 #.most-positive-fixnum)))

(defun storedp (object)
  (if *shared-objects*
      (values (gethash object *shared-objects*))
      (error 'without-context :name 'context)))

(defun store (object)
  (unless *shared-objects*
    (error 'without-context :name 'context))
  (when (should-do-p object)
    (let ((shared? (storedp object)))
      (if shared?
          (incf (count shared?))
          (progn
           (setf (gethash object *shared-objects*)
                   (make-shared :id *shared-count*))
           (incf *shared-count*)))))
  object)

(defun should-do-p (exp)
  (not
    (or (and (symbolp exp) (symbol-package exp))
        (characterp exp)
        (numberp exp))))

(defun only-once-p (exp)
  (let ((shared? (storedp exp)))
    (if shared?
        (zerop (count shared?))
        t)))

;;;; SEEN?

(defvar *seen?* nil)

(defmacro with-check-object-seen (() &body body)
  `(let ((*seen?* (or *seen?* (make-hash-table :test #'eq))))
     ,@body))

(defun already-printed-p (exp)
  (if *seen?*
      (values (gethash exp *seen?*))
      (error 'without-context :name 'with-check-object-seen)))

(defun mark-printed (exp)
  (if *seen?*
      (setf (gethash exp *seen?*) t)
      (error 'without-context :name 'with-check-object-seen)))

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

(set-pprint-dispatch '(cons (member context with-check-object-seen))
                     'pprint-context)