(defpackage :vivid-colors.spec
  (:use :cl :jingoh :vivid-colors)
  (:shadowing-import-from :vivid-colors "*PRINT-VIVID*")
  (:import-from :vivid-colors "VPRINT"))
(in-package :vivid-colors.spec)
(setup :vivid-colors)

(requirements-about common-lisp)

#?(subtypep '(cons (member quote)) 'list) => T

(requirements-about VPRINT :doc-type function)

;;;; Description:

#+syntax (VPRINT exp &optional output) ; => result

;;;; Arguments and Values:

; exp := t

; output := output-stream, otherwise implementation dependent condition.
#?(vprint t "not stream") :signals condition

; result := NIL

;;;; Affected By:
; *vprint-dispatch*

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
; Keyword symbols are printed as yellow.
#?(vprint :hoge)
:outputs #.(let ((cl-ansi-text:*color-mode* :8bit))
	     (cl-ansi-text:yellow ":HOGE"))

; Symbols are printed without color.
#?(vprint 'car)
:outputs "CAR"

#?(vprint '#:uninterned)
:outputs "#:UNINTERNED"

; NIL is printed as NIL, not ().
#?(vprint nil)
:outputs "NIL"

; Real is printed as violet.
#?(vprint 0)
:outputs #.(let ((cl-ansi-text:*color-mode* :8bit))
	     (with-output-to-string (out)
	       (cl-ansi-text:with-color (cl-colors2:+violet+ :stream out)
		 (princ 0 out))))

#?(vprint 0.1)
:outputs #.(let ((cl-ansi-text:*color-mode* :8bit))
	     (with-output-to-string (out)
	       (cl-ansi-text:with-color (cl-colors2:+violet+ :stream out)
		 (princ 0.1 out))))

#?(vprint 1/2)
:outputs #.(let ((cl-ansi-text:*color-mode* :8bit))
	     (with-output-to-string (out)
	       (cl-ansi-text:with-color (cl-colors2:+violet+ :stream out)
		 (princ 1/2 out))))

; Character is printed as lime.
#?(vprint #\a)
:outputs #.(let ((cl-ansi-text:*color-mode* :8bit))
	     (with-output-to-string (out)
	       (cl-ansi-text:with-color (cl-colors2:+limegreen+ :stream out)
		 (prin1 #\a out))))

; String is printed as tomato.
#?(vprint "tomato")
:outputs #.(let ((cl-ansi-text:*color-mode* :8bit))
	     (with-output-to-string (out)
	       (cl-ansi-text:with-color (cl-colors2:+tomato+ :stream out)
		 (prin1 "tomato" out))))

; QUOTE is printed as macro char.
#?(vprint ''car)
:outputs "'CAR"

; Exception situation: Some args.
#?(vprint '(quote a b c))
:outputs "(QUOTE A B C)"

; FUNCTION is printed as macro char.
#?(vprint '#'car)
:outputs "#'CAR"

; Exception situation: Some args.
#?(vprint '(function a b c))
:outputs "(FUNCTION A B C)"

; Pathname.
#?(vprint #P"tomato")
:outputs #.(let ((cl-ansi-text:*color-mode* :8bit))
	     (with-output-to-string (out)
	       (princ "#P" out)
	       (cl-ansi-text:with-color (cl-colors2:+tomato+ :stream out)
		 (prin1 "tomato" out))))

; LET form.
#?(vprint '(let))
:outputs "(LET)"

#?(vprint '(let . dot))
:outputs "(LET . DOT)"

#?(vprint '(let atom))
:outputs "(LET ATOM)"

#?(vprint '(let nil))
:outputs "(LET ())"

#?(vprint '(let (atom)))
:outputs "(LET (ATOM))"

#?(vprint '(let (atom . dot)))
:outputs "(LET (ATOM . DOT))"

#?(vprint '(let ((atom))))
:outputs "(LET ((ATOM)))"

#?(vprint '(let ((atom . dot))))
:outputs "(LET ((ATOM . DOT)))"

#?(vprint '(let ((too many elt))))
:outputs "(LET ((TOO MANY ELT)))"

#?(vprint '(let ((too many elt with . dot))))
:outputs "(LET ((TOO MANY ELT WITH . DOT)))"

#?(vprint '(let () . dot))
:outputs "(LET () . DOT)"

(requirements-about *PRINT-VIVID* :doc-type variable
		    :around (let ((*print-pretty* t))
			      (call-body)))
;;;; Description:

;;;; Value type is BOOLEAN
#? *PRINT-VIVID* :be-the BOOLEAN

; Initial value is `T`

;;;; Affected By:
; VPRINT.
#?(let ((*print-vivid* t))
    (vprint :hoge))
:outputs #.(let ((cl-ansi-text:*color-mode* :8bit))
	     (cl-ansi-text:yellow (prin1-to-string :hoge)))

#?(let ((*print-vivid* nil))
    (vprint :hoge))
:outputs ":HOGE"

;;;; Notes:
; This is control coloring only. Pretty printings is not its responds.
#?(let ((*print-vivid* nil)
	(*print-right-margin* 80))
    (vprint-logical-block (nil nil)
      (vprint-newline :mandatory *standard-output*)))
:outputs "
"

#?(let ((*print-pretty* nil))
    (vprint-logical-block (nil nil)
      (vprint-newline :mandatory *standard-output*)))
:outputs ""

