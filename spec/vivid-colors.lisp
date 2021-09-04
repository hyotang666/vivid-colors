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

(requirements-about PUT-STRINGS :doc-type function)

;;;; Description:
; Similar with WRITE-STRING but for partially colored string.

#+syntax (PUT-STRINGS strings output) ; => result

;;;; Arguments and Values:

; strings := list, otherwise implementation dependent condition.
#?(put-strings "not list" (make-instance 'vivid-colors::vprint-stream))
:signals condition
; List element must be a string-specifier,
; string-specifier := [ string | (cons string (cons cl-ansi-text:color-specifier (cons args))) ]
; otherwise an error is signaled.
#?(put-strings '(not string specifier) (make-instance 'vivid-colors::vprint-stream))
:signals error
#?(put-strings '((not-string #.cl-colors2:+red+)) (make-instance 'vivid-colors::vprint-stream))
:signals error
#?(put-strings '(("string" not-color-specifier)) (make-instance 'vivid-colors::vprint-stream))
:signals error

; args := [ :effect effect | :style style ]
; To see details of effect eval (mapcar #'car cl-ansi-text::+term-effects+).
; To see details of style eval (mapcar #'car cl-ansi-text::+text-style+).
#?(put-strings '(("string" #.cl-colors2:+red+ :unknown-key 0)) (make-instance 'vivid-colors::vprint-stream))
:signals error
#?(put-strings '(("string" #.cl-colors2:+red+ :effect :no-such-effect)) (make-instance 'vivid-colors::vprint-stream))
:signals error
#?(put-strings '(("string" #.cl-colors2:+red+ :style :no-such-style)) (make-instance 'vivid-colors::vprint-stream))
:signals error

; output := vprint-stream, otherwise implementation dependent condition.
#?(put-strings '("dummy") "not vprint stream") :signals condition

; result := null

;;;; Affected By:
; *PRINT-VIVID*
#?(let ((vs (make-instance 'vivid-colors::vprint-stream))
	(*print-vivid* nil))
    (put-strings '("1" ("2" #.cl-colors2:+red+)) vs)
    (values (copy-seq (vivid-colors::buffer vs))
	    (vivid-colors::view-length vs)))
:values ("\"12\"" 4)

;;;; Side-Effects:
; Modify VPRINT-STREAM state.
#?(let ((vs (make-instance 'vivid-colors::vprint-stream)))
    (put-strings '("1" ("2" #.cl-colors2:+red+)) vs)
    (values (copy-seq (vivid-colors::buffer vs))
	    (vivid-colors::view-length vs)))
:values (#.(concatenate 'string
			"\"1"
			(let ((cl-ansi-text:*color-mode* :8bit))
			  (cl-ansi-text:red "2"))
			"\"")
	 4)

;;;; Notes:
; For above side effect, you should use PUT-STRINGS instead of WRITE-STRING and/or CL-ANSI-TEXT:WITH-COLOR.

; Actual printing is done when FINISH-OUTPUT is called.

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

(requirements-about VPRINT-NEWLINE :doc-type function
		    :around (let ((*print-pretty* t))
			      (call-body)))

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
    (vprint-logical-block (nil nil)
      (vprint-newline :mandatory *standard-output*)))
:outputs "
"

; If :LINEAR is specified, newline is printed only if total length is greater than *print-right-margin*.
#?(let ((*print-right-margin* 80)
	(vivid-colors::*newlinep*))
    (vprint-logical-block (nil nil)
      (put #\a *standard-output*)
      (vprint-newline :linear *standard-output*)))
:outputs "#\\a"

#?(let ((*print-right-margin* 0)
	(vivid-colors::*newlinep*))
    (vprint-logical-block (nil nil)
      (put #\a *standard-output*)
      (vprint-newline :linear *standard-output*)))
:outputs "#\\a
"

; If :MISER is specified, newline is printed when section is over *print-right-margin* and over *print-miser-width*.
; Case 1: Not over miser width.
#?(let ((*print-right-margin* 80)
	(*print-miser-width* 0))
    (vprint-logical-block (nil nil)
      (put #\a *standard-output*)
      (vprint-newline :miser *standard-output*)))
:outputs "#\\a"

; Case 2: Over miser width, but not over right margin.
#?(let ((*print-right-margin* 80)
	(*print-miser-width* 80))
    (vprint-logical-block (nil nil)
      (put #\a *standard-output*)
      (vprint-newline :miser *standard-output*)))
:outputs "#\\a"

; Case 3: Over miser width and over right margin.
#?(let ((*print-right-margin* 0)
	(*print-miser-width* 80))
    (vprint-logical-block (nil nil)
      (put #\a *standard-output*)
      (vprint-newline :miser *standard-output*)))
:outputs "#\\a
"

; If :FILL is specified, newline is printed (a) when next is over *print-right-margin*.
#?(let ((*print-right-margin* 5))
    (vprint-logical-block (nil nil)
      (put #\a *standard-output*)
      (vprint-newline :fill *standard-output*)
      (put #\b *standard-output*)))
:outputs "#\\a
#\\b"

#?(let ((*print-right-margin* 80))
    (vprint-logical-block (nil nil)
      (put #\a *standard-output*)
      (vprint-newline :fill *standard-output*)
      (put #\b *standard-output*)))
:outputs "#\\a#\\b"

; (b) if over *print-right-margin*.
#?(let ((*print-right-margin* 2))
    (vprint-logical-block (nil nil)
      (put #\a *standard-output*)
      (vprint-newline :fill *standard-output*)
      (put #\b *standard-output*)))
:outputs "#\\a
#\\b"

; (c) if under miser printing.
#?(let ((*print-right-margin* 0)
	(*print-miser-width* 80))
    (vprint-logical-block (nil nil)
      (put #\a *standard-output*)
      (vprint-newline :fill *standard-output*)
      (put #\b *standard-output*)))
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

#+syntax (VPRINT-LOGICAL-BLOCK (<stream-var> <list> &key (prefix "") (suffix ""))
           &body
           body)
; => result

;;;; Arguments and Values:

; <stream-var> := symbol, otherwise implementation dependent condition.
#?(vprint-logical-block ("not symbol" nil)) :signals condition
; Must refer output stream, otherwise implementation dependent condition.
#?(let ((ref "not stream"))
    (vprint-logical-block (ref nil)))
:signals condition

; <list> := The form generates list.

; prefix := string, otherwise implementation dependent condition.
#?(vprint-logical-block (nil nil :prefix 'not-string)) :signals condition

; suffix := string, otherwise implementation dependent condition.
#?(vprint-logical-block (nil nil :suffix 'not-string)) :signals condition

; body := implicit progn.

; result := T

;;;; Affected By:
; vivid-colors::*vstream*
; If *VSTREAM* is unbound, VPRINT-STREAM is newly allocated.
#?(values (boundp 'vivid-colors::*vstream*)
	  (vprint-logical-block (nil nil)
            (boundp 'vivid-colors::*vstream*)))
:values (nil t)

; If *VSTREAM* is already bound, its section is modified.
#?(let* ((vs (make-instance 'vivid-colors::vprint-stream))
	 (vivid-colors::*vstream* vs)
	 (sec (vivid-colors::section vivid-colors::*vstream*)))
    (vprint-logical-block (nil nil)
      (values (eq vs vivid-colors::*vstream*)
	      (eq sec (vivid-colors::section vivid-colors::*vstream*)))))
:values (T NIL)

;;;; Side-Effects:
; Vivid notation is printed to <STREAM>.
#?(with-output-to-string (out)
    (vprint-logical-block (out nil)
      (princ "hoge" out)))
=> "hoge"
,:test equal

;;;; Notes:
; Insinde the VPRINT-LOGICAL-BLOCK body, VPRINT-POP AND VPRINT-EXIT-IF-LIST-EXHAUSTED is works.
#?(vprint-logical-block (nil (list 0 1 2))
    (vprint-pop))
=> 0

#?(vprint-logical-block (nil (list 0 1 2))
    (loop :for elt = (vprint-pop)
	  :do (put elt *standard-output*)
	  (vprint-exit-if-list-exhausted)))
:outputs "012"

;;;; Exceptional-Situations:
; When <LIST> bound by non-list value, it is VPRINTed.
#?(vprint-logical-block (nil 'not-list))
:outputs "NOT-LIST"

(requirements-about VPRINT-POP :doc-type function)

;;;; Description:

#+syntax (VPRINT-POP) ; => result

;;;; Arguments and Values:

; result := 

;;;; Affected By:
; Lexically inside the VPRINT-LOGICAL-BLOCK body, otherwise an error is signaled.
#?(vprint-pop) :signals program-error
,:lazy t

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about VPRINT-EXIT-IF-LIST-EXHAUSTED :doc-type function)

;;;; Description:

#+syntax (VPRINT-EXIT-IF-LIST-EXHAUSTED) ; => result

;;;; Arguments and Values:

; result := 

;;;; Affected By:
; Lexically inside the VPRINT-LOGICAL-BLOCK body, otherwise an error is signaled.
#?(vprint-exit-if-list-exhausted) :signals program-error
,:lazy t

;;;; Side-Effects:

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

(requirements-about SET-VPRINT-DISPATCH :doc-type function)

;;;; Description:

#+syntax (SET-VPRINT-DISPATCH type function &optional (priority 0)) ; => result

;;;; Arguments and Values:

; type := (or cons symbol) i.e. type-specifier, otherwise implementation dependent condition.
#?(set-vprint-dispatch "not type specifier" #'car) :signals condition
#?(set-vprint-dispatch 'unknown #'car) :signals error

; function := (or symbol function), otherwise implementation dependent condition.
#?(set-vprint-dispatch 'null "not function designator") :signals condition

; priority := real, otherwise implementation dependent condition.
#?(set-vprint-dispatch 'null #'car "not real") :signals condition

; result := (member t)

;;;; Affected By:

;;;; Side-Effects:
; Modify *VPRINT-DISPATCH*
#?(let ((*vprint-dispatch*))
    (set-vprint-dispatch 'null 'car)
    *vprint-dispatch*)
:satisfies (lambda (result)
	     (& (listp result)
		(= 1 (length result))
		(equalp (car result)
			(vivid-colors::make-vprinter :type 'null
						     :function 'car))))

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about COPY-VPRINT-DISPATCH :doc-type function)

;;;; Description:

#+syntax (COPY-VPRINT-DISPATCH &optional (vprint-dispatch *vprint-dispatch*))
; => result

;;;; Arguments and Values:
; COPY-PPRINT-DISPATCH for VPRINT-STREAM.

; vprint-dispatch := unspecified.
; If not specified, copy the value of *VPRINT-DISPATCH*.
#?(copy-vprint-dispatch)
:satisfies (lambda (result)
	     (& (equalp result *vprint-dispatch*)
		(not (eq result *vprint-dispatch*))))

; If specified NIL, default vprint-dispatch table is copied.
#?(let ((temp *vprint-dispatch*)
	(*vprint-dispatch*))
    (values (equalp *vprint-dispatch* (copy-vprint-dispatch nil))
	    (equalp temp (copy-vprint-dispatch nil))))
:values (NIL T)

; If vprint-dispatch is specified, such vprint-dispatch is copied.
#?(let ((table (list (vivid-colors::make-vprinter :type 'null :function 'car))))
    (values (eq table (copy-vprint-dispatch table))
	    (equalp table (copy-vprint-dispatch table))))
:values (NIL T)

; result := unspecified.

;;;; Affected By:
; *VPRINT-DISPATCH*.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

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
