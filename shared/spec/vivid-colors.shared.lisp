(defpackage :vivid-colors.shared.spec
  (:use :cl :jingoh :vivid-colors.shared)
  (:import-from :vivid-colors.shared #:storedp)
  (:shadowing-import-from :vivid-colors.shared count))
(in-package :vivid-colors.shared.spec)
(setup :vivid-colors.shared)

(requirements-about CONTEXT :doc-type function)

;;;; Description:
; Makind shared object context.

#?(values *shared-counter*
          vivid-colors.shared::*shared-objects*
          (context () *shared-counter*)
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
    (incf *shared-counter*)
    (context () (eql *shared-counter* 1)))
=> T

#+syntax (CONTEXT () &body body) ; => result

;;;; Arguments and Values:

; body := implicit progn.

; result := the return value of body.

;;;; Affected By:
; *SHARED-COUNTET*, *SHARED-OBJECTS*

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ID :doc-type function)

;;;; Description:

#+syntax (ID sb-kernel:instance &optional errorp) ; => result

;;;; Arguments and Values:

; instance := vivid-colors.shared::shared, otherwise implementation dependent condition.
#?(id "not shared") :signals condition

; errorp := boolean to specify signal an error when id is not initialized.
#?(id (vivid-colors.shared::make-shared)) => nil
#?(id (vivid-colors.shared::make-shared) t) :signals error

; result := (integer 0 #.most-positive-fixnum)

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about STORE :doc-type function)

;;;; Description:
; Evaluated to be true only if object actualy stored.

#+syntax (STORE object) ; => result

;;;; Arguments and Values:

; object := T

; result := BOOLEAN

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify *SHARED-OBJECTS*
#?(context ()
    (values (hash-table-count vivid-colors.shared::*shared-objects*)
            (store "dummy")
            (hash-table-count vivid-colors.shared::*shared-objects*)))
:values (0 T 1)

; When same (means (satisfies EQ)) object is stored some times,
; it is counted.
#?(let ((string "dummy"))
    (context ()
      (store string)
      (values (count (storedp string))
                (store string)
                (count (storedp string)))))
:values (1 NIL 2)

;;;; Notes:
; The symbols which is interned, numbers, and characters are not stored.
#?(context ()
    (values (hash-table-count vivid-colors.shared::*shared-objects*)
            *shared-counter*
            (store t)
            (store 0)
            (store #\a)
            (hash-table-count vivid-colors.shared::*shared-objects*)
            *shared-counter*))
:values (0 0 NIL NIL NIL 0 0)

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
#?(let ((string "dummy"))
    (context ()
      (store string)
      (storedp string)))
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

(requirements-about SHAREDP :doc-type function)

;;;; Description:
; Evaluated to be true when object is shared.

; Case not stored.
#?(context () (sharedp t)) => NIL

; Case stored only once.
#?(let ((string "dummy"))
    (context ()
      (store string)
      (sharedp string)))
=> NIL

; Case stored sometimes.
#?(let ((string "dummy"))
    (context ()
      (dotimes (x 2) (store string))
      (sharedp string)))
:be-the vivid-colors.shared::shared

#+syntax (SHAREDP exp &optional errorp) ; => result

;;;; Arguments and Values:

; exp := t

; errorp := boolean to control signal an error when not shared.
#?(context () (sharedp t t)) :signals error

; result := boolean.

;;;; Affected By:
; *SHARED-OBJECTS*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; If called without CONTEXT context, an error is signaled.
#?(sharedp t) :signals program-error
