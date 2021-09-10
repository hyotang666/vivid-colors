(in-package :vivid-colors)

(defstruct (queue (:constructor make-queue
                   (&key (type t) &aux (head (cons :head nil)) (tail head))))
  head
  (tail (error "TAIL is required."))
  (type t :type (satisfies millet:type-specifier-p) :read-only t))

(defun (setf tail) (new queue)
  (assert (typep new (queue-type queue)))
  (rplacd (queue-tail queue) (setf (queue-tail queue) (list new)))
  new)

(defun count-cons (cons)
  (labels ((rec (cons count)
             (if (atom cons)
                 count
                 (rec (cdr cons) (1+ count)))))
    (rec cons 0)))

(defmacro doqueue ((var <queue> &optional <return>) &body body)
  (let ((vars (uiop:ensure-list var)))
    `(loop :for ,vars :on (cdr (queue-head ,<queue>))
                :by (lambda (x) (nthcdr ,(count-cons vars) x))
           :do (tagbody ,@body)
           :finally (return ,<return>))))
