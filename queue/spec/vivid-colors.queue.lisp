(defpackage :vivid-colors.queue.spec
  (:use :cl :jingoh :vivid-colors.queue))
(in-package :vivid-colors.queue.spec)
(setup :vivid-colors.queue)

(requirements-about QUEUE :doc-type STRUCTURE)

;;;; Description:
; Simple queue.

;;;; Class Precedence List: (case in SBCL)
; queue structure-object slot-object t

;;;; Effective Slots:

; HEAD [Type] T

; TAIL [Type] T

; TYPE [Type] (SATISFIES TYPE-SPECIFIER-P)

;;;; Notes:

(requirements-about NEW :doc-type function)

;;;; Description:
; Constructor.

#+syntax (NEW &key (type t)) ; => result

;;;; Arguments and Values:

; type := type-specifier, otherwise implementation dependent condition.
#?(new :type :unknown-type) :signals condition

; result := queue

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about CONTENTS :doc-type function)

;;;; Description:
; Retrieve all queue body as list.

#+syntax (CONTENTS queue) ; => result

;;;; Arguments and Values:

; queue := queue, otherwise implementation dependent condition.
#?(contents "not queue") :signals condition

; result := list

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about FOR-EACH :doc-type function)

;;;; Description:

#+syntax (FOR-EACH (var <queue> &optional <return>) &body body) ; => result

;;;; Arguments and Values:

; var := [ symbol | (symbol . var) ], otherwise implementation dependent condition.
#?(for-each ("not symbol" (new))) :signals condition
; Not evaluated.
#?(for-each ((intern "not evaluated") (new))) :signals condition
; Var is bound each content of queue.
#?(let ((q (new)))
    (setf (tail q) :a)
    (for-each (content q) (print content)))
:outputs "
:A "

; When var is CONS, queue contents iterate by nthcdr of it.
#?(let ((q (new)))
    (setf (tail q) 0
	  (tail q) 1
	  (tail q) 2
	  (tail q) 3)
    (for-each ((a b) q)
      (print (+ a b))))
:outputs "
1 
5 "

; <queue> := The form that generates queue object.
#?(for-each (var "not queue")) :signals condition

; <return> := The form evaluated at last.
#?(let ((q (new)))
    (setf (tail q) :a)
    (for-each (v q (print :last))
      (princ v)))
:outputs "A
:LAST "

; body := Implicit progn.

; result := The return value of <return>.
#?(for-each (v (new) :this-is-returned)
    (declare (ignore v)))
=> :THIS-IS-RETURNED

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

