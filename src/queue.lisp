(in-package :vivid-colors)

(defstruct (queue (:constructor make-queue
                   (&aux (head (cons :head nil)) (tail head))))
  head
  (tail (error "TAIL is required.")))

(defun (setf tail) (new queue)
  (rplacd (queue-tail queue) (setf (queue-tail queue) (list new)))
  new)

(defun count-cons (cons)
  (labels ((rec (cons count)
             (if (atom cons)
                 count
                 (rec (cdr cons) (1+ count)))))
    (rec cons 0)))

(defmacro doqueue ((var <queue> &optional <return>) &body body)
  `(loop :for ,(uiop:ensure-list var) :on (cdr (queue-head ,<queue>))
              :by (lambda (x) (nthcdr ,(count-cons var) x))
         :do (tagbody ,@body)
         :finally (return ,<return>)))