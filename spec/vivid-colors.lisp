(defpackage :vivid-colors.spec
  (:use :cl :jingoh :vivid-colors)
  (:shadowing-import-from :vivid-colors "*PRINT-VIVID*")
  (:import-from :vivid-colors "VPRINT"))
(in-package :vivid-colors.spec)
(setup :vivid-colors)

(requirements-about PUT-CHAR :doc-type function)

;;;; Description:
; WRITE-CHAR for VPRINT-STREAM.

#+syntax (PUT-CHAR char output) ; => result

;;;; Arguments and Values:

; char := character, otherwise implementation dependent condition.
#?(put-char "not character" (make-instance 'vivid-colors::vprint-stream))
:signals condition

; output := vprint-stream, otherwise implementation dependent condition.
#?(put-char #\a "not vprint-stream") :signals condition

; result := character

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify vprint-stream state.
#?(let ((vs (make-instance 'vivid-colors::vprint-stream)))
    (values (copy-seq (vivid-colors::buffer vs))
	    (vivid-colors::view-length vs)
	    (put-char #\a vs)
	    (copy-seq (vivid-colors::buffer vs))
	    (vivid-colors::view-length vs)))
:values ("" 0 #\a "a" 1)

;;;; Notes:
; Because above side effect, you should use PUT-CHAR rather than WRITE-CHAR for VPRINT-STREAM.

;;;; Exceptional-Situations:

(requirements-about PUT :doc-type function)

;;;; Description:
; WRITE for VPRINT-STREAM.

#+syntax (PUT object output &key color (key #'prin1-to-string)) ; => result

;;;; Arguments and Values:

; object := t

; output := vprint-stream, otherwise implementation dependent condition.
#?(put t "not vprint stream") :signals condition

; color := (or null cl-ansi-text:color-specifier), otherwise implementation dependent condition.
#?(put t (make-instance 'vivid-colors::vprint-stream) :color "not color specifier")
:signals condition

; key := (or symbol function), otherwise implementation dependent condition.
#?(put t (make-instance 'vivid-colors::vprint-stream) :key "not function designator")
:signals condition
; KEY is as (function (object) string), if return type is not string, an error is signaled.
#?(put t (make-instance 'vivid-colors::vprint-stream) :key #'identity)
:signals error

; result := object

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify VPRINT-STREAM state.
#?(let ((vs (make-instance 'vivid-colors::vprint-stream)))
    (values (copy-seq (vivid-colors::buffer vs))
	    (vivid-colors::view-length vs)
	    (put #\a vs)
	    (copy-seq (vivid-colors::buffer vs))
	    (vivid-colors::view-length vs)))
:values ("" 0 #\a "#\\a" 3)

#?(let ((vs (make-instance 'vivid-colors::vprint-stream)))
    (values (copy-seq (vivid-colors::buffer vs))
	    (vivid-colors::view-length vs)
	    (put #\a vs :color cl-colors2:+red+)
	    (copy-seq (vivid-colors::buffer vs))
	    (vivid-colors::view-length vs)))
:values ("" 0 #\a #.(let ((cl-ansi-text:*color-mode* :8bit))
		      (cl-ansi-text:red  (prin1-to-string #\a))) 3)

;;;; Notes:
; Because above side effect, you should use PUT rather than WRITE for VPRINT-STREAM.

;;;; Exceptional-Situations:

(requirements-about VPRINT-NEWLINE :doc-type function)

;;;; Description:

#+syntax (VPRINT-NEWLINE kind output) ; => result

;;;; Arguments and Values:

; kind := (member :miser :fill :linear :mandatory nil), otherwise implementation dependent condition.
#?(vprint-newline "not member" (make-instance 'vivid-colors::vprint-stream))
:signals condition

; output := vprint-stream, otherwise implementation dependent condition.
#?(vprint-newline nil "not vprint stream") :signals condition

; result := (VALUES)

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify VPRINT-STREAM state.
#?(let ((vs (make-instance 'vivid-colors::vprint-stream)))
    (vprint-newline :linear vs)
    (cdr (vivid-colors::queue-head (vivid-colors::lines (vivid-colors::section vs)))))
:satisfies (lambda (queue)
	     (& (listp queue)
		(= 2 (length queue))
		(equalp (first queue)
			(vivid-colors::make-line :contents "" :length 0))
		(eq :linear (second queue))))

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
; If :MANDATORY specified, newline is printed even if the form is short.
#?(let ((*print-right-margin* 80))
    (vprint-logical-block (out nil)
      (vprint-newline :mandatory out)))
:outputs "
"

; If :LINEAR is specified, newline is printed only if total length is greater than *print-right-margin*.
#?(let ((*print-right-margin* 80)
	(vivid-colors::*newlinep*))
    (vprint-logical-block (out nil)
      (put #\a out)
      (vprint-newline :linear out)))
:outputs "#\\a"

#?(let ((*print-right-margin* 0)
	(vivid-colors::*newlinep*))
    (vprint-logical-block (out nil)
      (put #\a out)
      (vprint-newline :linear out)))
:outputs "#\\a
"

; If :MISER is specified, newline is printed when section is over *print-right-margin* and over *print-miser-width*.
; Case 1: Not over miser width.
#?(let ((*print-right-margin* 80)
	(*print-miser-width* 0))
    (vprint-logical-block (out nil)
      (put #\a out)
      (vprint-newline :miser out)))
:outputs "#\\a"

; Case 2: Over miser width, but not over right margin.
#?(let ((*print-right-margin* 80)
	(*print-miser-width* 80))
    (vprint-logical-block (out nil)
      (put #\a out)
      (vprint-newline :miser out)))
:outputs "#\\a"

; Case 3: Over miser width and over right margin.
#?(let ((*print-right-margin* 0)
	(*print-miser-width* 80))
    (vprint-logical-block (out nil)
      (put #\a out)
      (vprint-newline :miser out)))
:outputs "#\\a
"

; If :FILL is specified, newline is printed (a) when next is over *print-right-margin*.
#?(let ((*print-right-margin* 5))
    (vprint-logical-block (out nil)
      (put #\a out)
      (vprint-newline :fill out)
      (put #\b out)))
:outputs "#\\a
#\\b"

#?(let ((*print-right-margin* 80))
    (vprint-logical-block (out nil)
      (put #\a out)
      (vprint-newline :fill out)
      (put #\b out)))
:outputs "#\\a#\\b"

; (b) if over *print-right-margin*.
#?(let ((*print-right-margin* 2))
    (vprint-logical-block (out nil)
      (put #\a out)
      (vprint-newline :fill out)
      (put #\b out)))
:outputs "#\\a
#\\b"

; (c) if under miser printing.
#?(let ((*print-right-margin* 0)
	(*print-miser-width* 80))
    (vprint-logical-block (out nil)
      (put #\a out)
      (vprint-newline :fill out)
      (put #\b out)))
:outputs "#\\a
#\\b"

(requirements-about VPRINT-INDENT :doc-type function)

;;;; Description:

#+syntax (VPRINT-INDENT kind indent output) ; => result

;;;; Arguments and Values:

; kind := (member :block :current), otherwise implementation dependent condition.
#?(vprint-indent "not member" 0 (make-instance 'vivid-colors::vprint-stream))
:signals condition

; indent := (unsigned-byte 62), otherwise implementation dependent condition.
#?(vprint-indient :block "not unsigned-byte" (make-instance 'vivid-colors::vprint-stream))
:signals condition

; output := vprint-stream, otherwise implementation dependent condition.
#?(vprint-indent :blocl 0 "not vprint stream") :signals condition

; result := (values)

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify VPRINT-STREAM state.
#?(let ((vs (make-instance 'vivid-colors::vprint-stream)))
    (values (vivid-colors::indent (vivid-colors::section vs))
	    (progn (vprint-indent :block 3 vs)
		   (vivid-colors::indent (vivid-colors::section vs)))))
:values (0 3)

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about VPRINT-LOGICAL-BLOCK :doc-type function)

;;;; Description:
; PPRINT-LOGICAL-BLOCK for VPRINT-STREAM.

#+syntax (VPRINT-LOGICAL-BLOCK (var <stream> &key (prefix "") (suffix ""))
           &body
           body)
; => result

;;;; Arguments and Values:

; var := symbol, otherwise implementation dependent condition.
#?(vprint-logical-block ("not symbol" nil)) :signals condition
; Not evaluated.
#?(vprint-logical-block ((intern "not evaluated.") nil)) :signals condition

; <stream> := The form that generates output stream designator, otherwise type error.
#?(vprint-logical-block (var "not output stream")) :signals type-error

; prefix := string, otherwise implementation dependent condition.
#?(vprint-logical-block (var nil :prefix 'not-string)) :signals condition

; suffix := string, otherwise implementation dependent condition.
#?(vprint-logical-block (var nil :suffix 'not-string)) :signals condition

; body := implicit progn.

; result := T

;;;; Affected By:
; vivid-colors::*vstream*
; If *VSTREAM* is unbound, VPRINT-STREAM is newly allocated.
#?(values (boundp 'vivid-colors::*vstream*)
	  (vprint-logical-block (var nil)
            (boundp 'vivid-colors::*vstream*)))
:values (nil t)

; If *VSTREAM* is already bound, its section is modified.
#?(let* ((vs (make-instance 'vivid-colors::vprint-stream))
	 (vivid-colors::*vstream* vs)
	 (sec (vivid-colors::section vivid-colors::*vstream*)))
    (vprint-logical-block (var nil)
      (values (eq vs vivid-colors::*vstream*)
	      (eq sec (vivid-colors::section vivid-colors::*vstream*)))))
:values (T NIL)

;;;; Side-Effects:
; Vivid notation is printed to <STREAM>.
#?(with-output-to-string (out)
    (vprint-logical-block (v out)
      (princ "hoge" v)))
=> "hoge"
,:test equal

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about *VPRINT-DISPATCH* :doc-type variable)

;;;; Description:
; CL:*PRINT-PPRINT-DISPATCH* for VPRINT-STREAM.

;;;; Value type is unspecified.
#? *VPRINT-DISPATCH* => unspecified

;;;; Affected By:
; set-vprint-dispatch, copy-vprint-dispatch

;;;; Notes:

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
