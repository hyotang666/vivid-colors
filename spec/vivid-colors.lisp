(defpackage :vivid-colors.spec
  (:use :cl :jingoh :vivid-colors)
  (:shadowing-import-from :vivid-colors "*PRINT-VIVID*")
  (:import-from :vivid-colors "VPRINT"))
(in-package :vivid-colors.spec)
(setup :vivid-colors)

(requirements-about common-lisp)

#?(subtypep '(cons (member quote)) 'list) => T

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

#+syntax (SET-VPRINT-DISPATCH type
           function
           &optional
           (priority 0)
           (table *vprint-dispatch*))
; => result

;;;; Arguments and Values:

; type := (or cons symbol) i.e. type-specifier, otherwise implementation dependent condition.
#?(set-vprint-dispatch "not type specifier" #'car) :signals condition
#?(set-vprint-dispatch 'unknown #'car) :signals error

; function := (or symbol function), otherwise implementation dependent condition.
#?(set-vprint-dispatch 'null "not function designator") :signals condition

; priority := real, otherwise implementation dependent condition.
#?(set-vprint-dispatch 'null #'car "not real") :signals condition

; table := vprint-dispatch, otherwise implementation dependent condition.
#?(set-vprint-dispatch 'null #'car 0 "not vprint dispatch") :signals condition

; result := null

;;;; Affected By:

;;;; Side-Effects:
; Modify *VPRINT-DISPATCH*
#?(let ((*vprint-dispatch* (vivid-colors::make-vprint-dispatch :name :dummy)))
    (set-vprint-dispatch 'null 'car)
    *vprint-dispatch*)
:satisfies (lambda (result)
	     (& (typep result 'vivid-colors::vprint-dispatch)
		(= 1 (hash-table-count (vivid-colors::vprint-dispatch-table result)))
		(equalp (gethash 'null (vivid-colors::vprint-dispatch-table result))
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
	(*vprint-dispatch* (vivid-colors::make-vprint-dispatch :name :dummy)))
    (values (equalp *vprint-dispatch* (copy-vprint-dispatch nil))
	    (equalp temp (copy-vprint-dispatch nil))))
:values (NIL T)

; If vprint-dispatch is specified, such vprint-dispatch is copied.
#?(let ((table (vivid-colors::make-vprint-dispatch
		 :name :dummy
		 :table (alexandria:plist-hash-table
			  (list 'null (vivid-colors::make-vprinter :type 'null :function 'car))))))
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

(requirements-about FIND-VPRINT-DISPATCH :doc-type function)

;;;; Description:

#+syntax (FIND-VPRINT-DISPATCH name) ; => result

;;;; Arguments and Values:

; name := symbol, otherwise implementation dependent condition.
#?(find-vprint-dispatch "not symbol") :signals condition

; result := (or null vprint-dispatch)

;;;; Affected By:
; vivid-colors::*vprint-dispatch-repositoty*.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about STORE-VPRINT-DISPATCH :doc-type function)

;;;; Description:

#+syntax (STORE-VPRINT-DISPATCH name vprint-dispatch) ; => result

;;;; Arguments and Values:

; name := symbol, otherwise implementation dependent condition.
#?(store-vprint-dispatch "not symbol" (vivid-colors::make-vprint-dispatch :name :dummy))
:signals condition

; vprint-dispatch := vprint-dispatch, otherwise implementation dependent condition.
#?(store-vprint-dispatch :dummy "not vprint dispatch") :signals condition

; result := vprint-dispatch

;;;; Affected By:

;;;; Side-Effects:
; Modify vivid-colors::*vprint-dispatch-repository*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MERGE-VPRINT-DISPATCH :doc-type function)

;;;; Description:

#+syntax (MERGE-VPRINT-DISPATCH vprint-dispatch &rest rest) ; => result

;;;; Arguments and Values:

; vprint-dispatch := vprint-dispatch, otherwise implementation dependent condition.
#?(merge-vprint-dispatch "not vprint-dispatch") :signals condition

; rest := vprint-dispatch, otherwise implementation dependent condition.
#?(merge-vprint-dispatch (vivid-colors::make-vprint-dispatch :name :dummy)
			 "not vprint-dispatch")
:signals condition

; result := vprint-dispatch

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:
; Always newly allocated vprint-dispatch is returned.
#?(let ((vprint-dispatch (vivid-colors::make-vprint-dispatch :name :dummy)))
    (eq vprint-dispatch (merge-vprint-dispatch vprint-dispatch)))
=> NIL

;;;; Exceptional-Situations:
; When same key-type is exists, an error is signaled.
#?(let ((*vprint-dispatch* (vivid-colors::make-vprint-dispatch
			     :name :dummy
			     :table (alexandria:plist-hash-table
				      (list 'null (vivid-colors::make-vprinter
						    :type 'null
						    :function 'list))))))
    (merge-vprint-dispatch *vprint-dispatch* (find-vprint-dispatch :standard)))
:signals error
,:with-restarts (replace ignore)

(requirements-about DEFINE-VPRINT-DISPATCH :doc-type function)

;;;; Description:

#+syntax (DEFINE-VPRINT-DISPATCH name &body clause+) ; => result

;;;; Arguments and Values:

; name := symbol, otherwise implementation dependent condition.
#?(define-vprint-dispatch "not symbol") :signals condition

; clause+ := [ set-clause | merge-clause ], otherwise implementation dependent condition.
#?(define-vprint-dispatch :dummy :unknown-form) :signals condition
; set-clause := (:set type-specifier function), See SET-VPRINT-DISPATCH.
; merge-clause := (:merge name*)
; If specified vprint-dispatch is not found, an error is signaled.
#?(define-vprint-dispatch :dummy (:merge :unknown)) :signals error

; result := name.

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify vivid-colors::*vprint-dispatch-repository*.
#?(let ((vivid-colors::*vprint-dispatch-repository* (make-hash-table)))
    (values (hash-table-count vivid-colors::*vprint-dispatch-repository*)
	    (progn (define-vprint-dispatch :dummy)
		   (hash-table-count vivid-colors::*vprint-dispatch-repository*))))
:values (0 1)

;;;; Notes:

;;;; Exceptional-Situations:
; When some merge-clause exists, implementation dependent condition.
#?(define-vprint-dispatch :dummy (:merge) (:merge)) :signals condition

(requirements-about IN-VPRINT-DISPATCH :doc-type function)

;;;; Description:

#+syntax (IN-VPRINT-DISPATCH name) ; => result

;;;; Arguments and Values:

; name := symbol, otherwise implementation dependent condition.
#?(in-vprint-dispatch "not symbol") :signals condition
; When specified vprint-dispatch is not found, an error is signaled.
#?(in-vprint-dispatch :unknown) :signals error

; result := vprint-dispatch

;;;; Affected By:
; vivid-colors::*vprint-dispatch-repository*

;;;; Side-Effects:
; Modify *vprint-dispatch*.
#?(let ((*vprint-dispatch* *vprint-dispatch*))
    (values (vivid-colors::vprint-dispatch-name *vprint-dispatch*)
	    (progn (in-vprint-dispatch :vivid)
		   (vivid-colors::vprint-dispatch-name *vprint-dispatch*))))
:values (:STANDARD :VIVID)

;;;; Notes:

;;;; Exceptional-Situations:

