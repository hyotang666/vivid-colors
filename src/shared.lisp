(in-package :vivid-colors)

;;;; SHARED.
;; To support *print-circle*

(declaim (type (or null (integer 0 #.most-positive-fixnum)) *shared-count*))

(defparameter *shared-count* 0)

(declaim
 (type (or null hash-table) ; :TEST must be #'EQ.
  *shared-objects*))

(defparameter *shared-objects* nil)

(defvar *seen?* nil)

(defmacro with-check-object-seen (() &body body)
  `(let ((*seen?* (or *seen?* (make-hash-table :test #'eq))))
     ,@body))

(defstruct (shared (:predicate nil))
  (id (error "ID is required.")
      :type (integer 0 #.most-positive-fixnum)
      :read-only t)
  (count 0 :type (integer 0 #.most-positive-fixnum)))

(defun shared-p (object) (values (gethash object *shared-objects*)))

(defun store-shared-object (object)
  (when (should-be-shared-p object)
    (setf (gethash object *shared-objects*) (make-shared :id *shared-count*))
    (incf *shared-count*))
  object)

(defun increase-count (exp)
  (incf (shared-count (gethash exp *shared-objects*))))

(defun should-be-shared-p (exp)
  (not
    (or (and (symbolp exp) (symbol-package exp))
        (characterp exp)
        (numberp exp))))

(defun only-once-p (exp)
  (let ((shared? (shared-p exp)))
    (if shared?
        (zerop (shared-count shared?))
        t)))

(defun already-printed-p (exp) (values (gethash exp *seen?*)))

(defun mark-printed (exp) (setf (gethash exp *seen?*) t))