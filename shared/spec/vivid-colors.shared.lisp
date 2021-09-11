(defpackage :vivid-colors.shared.spec
  (:use :cl :jingoh :vivid-colors.shared)
  (:shadowing-import-from :vivid-colors.shared count))
(in-package :vivid-colors.shared.spec)
(setup :vivid-colors.shared)

(requirements-about CONTEXT :doc-type function)

;;;; Description:
; Makind shared object context.

#?(values vivid-colors.shared::*shared-count*
	  vivid-colors.shared::*shared-objects*
	  (context () vivid-colors.shared::*shared-count*)
	  (context () vivid-colors.shared::*shared-objects*))
:multiple-value-satisfies
(lambda (first second third fourth)
  (& (null first)
     (null second)
     (eql 0 third)
     (hash-table-p fourth)))

; Only one context is made.
#?(context ()
    (eq vivid-colors.shared::*shared-objects*
	(context () vivid-colors.shared::*shared-objects*)))
=> T

#?(context ()
    (incf vivid-colors.shared::*shared-count*)
    (context () (eql vivid-colors.shared::*shared-count* 1)))
=> T

#+syntax (CONTEXT () &body body) ; => result

;;;; Arguments and Values:

; body := implicit progn.

; result := the return value of body.

;;;; Affected By:
; *SHARED-COUNT*, *SHARED-OBJECTS*

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ID :doc-type function)

;;;; Description:

#+syntax (ID sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := vivid-colors.shared::shared, otherwise implementation dependent condition.
#?(id "not shared") :signals condition

; result := (integer 0 #.most-positive-fixnum)

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about STORE :doc-type function)

;;;; Description:

#+syntax (STORE object) ; => result

;;;; Arguments and Values:

; object := T

; result := object.

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify *SHARED-OBJECTS*, *SHARED-COUNT*.
#?(context ()
    (values (hash-table-count vivid-colors.shared::*shared-objects*)
	    vivid-colors.shared::*shared-count*
	    (store "dummy")
            (hash-table-count vivid-colors.shared::*shared-objects*)
	    vivid-colors.shared::*shared-count*))
:values (0 0 "dummy" 1 1)

; When same (means (satisfies EQ)) object is stored some times,
; it is counted.
#?(context ()
    (store "")
    (values (count (storedp ""))
	    (store "")
	    (count (storedp ""))))
:values (0 "" 1)

;;;; Notes:
; The symbols which is interned, numbers, and characters are not stored.
#?(context ()
    (values (hash-table-count vivid-colors.shared::*shared-objects*)
	    vivid-colors.shared::*shared-count*
	    (store t)
	    (store 0)
	    (store #\a)
            (hash-table-count vivid-colors.shared::*shared-objects*)
	    vivid-colors.shared::*shared-count*))
:values (0 0 t 0 #\a 0 0)

;;;; Exceptional-Situations:
; If called without CONTEXT context, an error is signaled.
#?(store t) :signals program-error

(requirements-about STOREDP :doc-type function)

;;;; Description:

#+syntax (STOREDP object) ; => result

;;;; Arguments and Values:

; object := T

; result := (or null vivid-colors.shared::shared)
#?(context () (storedp t)) => NIL
#?(context ()
    (store "")
    (storedp ""))
:be-the vivid-colors.shared::shared

;;;; Affected By:
; *SHARED-OBJECTS*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; If called without CONTEXT context, an error is signaled.
#?(storedp t) :signals program-error

(requirements-about SHOULD-DO-P :doc-type function)

;;;; Description:
; Evaluated to be NIL when EXP is interned symbols, numbers or characters.
#?(should-do-p t) => NIL
#?(should-do-p 0) => NIL
#?(should-do-p 1.2) => NIL
#?(should-do-p 1/2) => NIL
#?(should-do-p #C(1 2)) => NIL
#?(should-do-p #\C) => NIL
#?(should-do-p '#:uninterned) => T
#?(should-do-p '(cons . cons)) => T

#+syntax (SHOULD-DO-P exp) ; => result

;;;; Arguments and Values:

; exp := t

; result := boolean.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ONLY-ONCE-P :doc-type function)

;;;; Description:
; Evaluated to be true when object is not shared.

; Case not stored.
#?(context () (only-once-p t)) => T

; Case stored only once.
#?(context () (store "") (only-once-p "")) => T

; Case stored sometimes.
#?(context () (dotimes (x 2) (store ""))
    (only-once-p ""))
=> NIL

#+syntax (ONLY-ONCE-P exp) ; => result

;;;; Arguments and Values:

; exp := t

; result := boolean.

;;;; Affected By:
; *SHARED-OBJECTS*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; If called without CONTEXT context, an error is signaled.
#?(only-once-p t) :signals program-error

(requirements-about WITH-CHECK-OBJECT-SEEN :doc-type function)

;;;; Description:
; Making context.
#?(values vivid-colors.shared::*seen?*
	  (with-check-object-seen ()
            vivid-colors.shared::*seen?*))
:multiple-value-satisfies
(lambda (first second)
  (& (null first)
     (hash-table-p second)))

; Once only.
#?(with-check-object-seen ()
    (eq vivid-colors.shared::*seen?*
	(with-check-object-seen ()
          vivid-colors.shared::*seen?*)))
=> T

#+syntax (WITH-CHECK-OBJECT-SEEN () &body body) ; => result

;;;; Arguments and Values:

; body := Implicit progn.

; result := Return values of the body.

;;;; Affected By:
; *SEEN?*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ALREADY-PRINTED-P :doc-type function)

;;;; Description:
; Evaluated to be true if EXP is already seen.

#?(with-check-object-seen ()
    (already-printed-p t))
=> NIL

#?(with-check-object-seen ()
    (mark-printed t)
    (already-printed-p t))
=> T

#+syntax (ALREADY-PRINTED-P exp) ; => result

;;;; Arguments and Values:

; exp := t

; result := boolean.

;;;; Affected By:
; *SEEN?*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; If called without WITH-CHCECK-OBJECT-SEEN context, an error is signaled.
#?(already-printed-p t) :signals program-error

(requirements-about MARK-PRINTED :doc-type function)

;;;; Description:
; Mark EXP as already seen.

#+syntax (MARK-PRINTED exp) ; => result

;;;; Arguments and Values:

; exp := t

; result := (eql t)

;;;; Affected By:

;;;; Side-Effects:
; Modify *SEEN?*

#?(with-check-object-seen ()
    (values (hash-table-count vivid-colors.shared::*seen?*)
	    (mark-printed nil)
	    (hash-table-count vivid-colors.shared::*seen?*)))
:values (0 T 1)

;;;; Notes:

;;;; Exceptional-Situations:
; If called without with-check-object-seen context, an error is signaled.
#?(mark-printed nil) :signals program-error
