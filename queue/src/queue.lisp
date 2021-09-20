(in-package :cl-user)

(defpackage :vivid-colors.queue
  (:use :cl)
  (:export #:queue ; Structure name.
           #:new ; Constructor.
           #:contents ; Reader.
           #:tail ; Modifier.
           #:for-each ; Iterator.
           )
  (:documentation "Provide the queue data structure as a module for vivid-colors."))

(in-package :vivid-colors.queue)

(declaim (optimize speed))

;;; The QUEUE data structure that is efficient rather than push/reverse or vector-push-extend.

(defstruct (queue (:constructor new
                   (&key (type t) &aux (head (cons :head nil)) (tail head)
                    (type ; clisp needs.
                     (progn (assert (millet:type-specifier-p type)) type)))))
  head
  (tail (error "TAIL is required."))
  (type t :type (satisfies millet:type-specifier-p) :read-only t))

;;; An abstraction barriar as UPDATOR.

(defun (setf tail) (new queue)
  (locally ; Due to type is known in runtime.
   (declare (optimize (speed 1)))
   (assert (typep new (queue-type queue))))
  (rplacd (queue-tail queue) (setf (queue-tail queue) (list new)))
  new)

;;; An abstraction barriar as REFERER.

(defun contents (queue) (cdr (queue-head queue)))

;;; An abstraction barriar as ITERATOR.

(defun count-cons (cons)
  (labels ((rec (cons count)
             (if (atom cons)
                 count
                 (rec (cdr cons) (1+ count)))))
    (declare
      (ftype (function (t (mod #.most-positive-fixnum))
              (values (mod #.most-positive-fixnum) &optional))
             rec))
    (rec cons 0)))

(defmacro for-each ((var <queue> &optional <return>) &body body)
  (let ((vars (alexandria:ensure-list var)))
    `(loop :for ,vars :on (cdr (queue-head ,<queue>))
                :by (lambda (x) (nthcdr ,(count-cons vars) x))
           ;; In order to be declarable.
           ;; Not efficient but I do not want to reimplement loop destructuring feature.
           :do (destructuring-bind
                   ,(alexandria:flatten vars)
                   (list ,@(alexandria:flatten vars))
                 ,@body)
           :finally (return ,<return>))))