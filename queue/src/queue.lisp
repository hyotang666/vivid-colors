(in-package :cl-user)

(defpackage :vivid-colors.queue
  (:use :cl)
  (:export #:queue ; Structure name.
           #:new ; Constructor.
           #:contents ; Reader.
           #:tail ; Modifier.
           #:for-each ; Iterator.
           ))

(in-package :vivid-colors.queue)

(declaim (optimize speed))

(defstruct (queue (:constructor new
                   (&key (type t) &aux (head (cons :head nil)) (tail head))))
  head
  (tail (error "TAIL is required."))
  (type t :type (satisfies millet:type-specifier-p) :read-only t))

(defun (setf tail) (new queue)
  (locally ; Due to type is known in runtime.
   (declare (optimize (speed 1)))
   (assert (typep new (queue-type queue))))
  (rplacd (queue-tail queue) (setf (queue-tail queue) (list new)))
  new)

(defun contents (queue) (cdr (queue-head queue)))

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
  (let ((vars (uiop:ensure-list var)))
    `(loop :for ,vars :on (cdr (queue-head ,<queue>))
                :by (lambda (x) (nthcdr ,(count-cons vars) x))
           :do (tagbody ,@body)
           :finally (return ,<return>))))
