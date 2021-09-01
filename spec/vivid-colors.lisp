(defpackage :vivid-colors.spec
  (:use :cl :jingoh :vivid-colors)
  (:shadowing-import-from :vivid-colors "*PRINT-VIVID*")
  (:import-from :vivid-colors "VPRINT"))
(in-package :vivid-colors.spec)
(setup :vivid-colors)

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

; VPRINT increase VIEW-POSITION.
#?(let ((vivid-colors::*vstream*
	  (make-instance 'vivid-colors::vprint-stream
			 :output *standard-output*
			 :prefix ""
			 :suffix "")))
    (vprint 'car)
    (dev:peep vivid-colors::*vstream*)
    (vivid-colors::compute-length (vivid-colors::section vivid-colors::*vstream*)))
=> 3
,:stream nil
