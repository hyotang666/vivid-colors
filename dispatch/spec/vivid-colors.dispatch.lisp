(defpackage :vivid-colors.dispatch.spec
  (:import-from :vivid-colors.dispatch #:set-vprint-dispatch #:copy-vprint-dispatch
		#:store-vprint-dispatch #:merge-vprint-dispatch)
  (:use :cl :jingoh :vivid-colors.dispatch))
(in-package :vivid-colors.dispatch.spec)
(setup :vivid-colors.dispatch)

(requirements-about common-lisp)

#-ecl
#?(subtypep '(cons (member quote)) 'list) => T
;; as guard for ecl.
#+ecl
#?(subtypep '(cons (member quote)) 'list) => NIL

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
#?(let ((*vprint-dispatch* (vivid-colors.dispatch::make-vprint-dispatch :name :dummy)))
    (set-vprint-dispatch 'null 'car)
    *vprint-dispatch*)
:satisfies (lambda (result)
	     (& (typep result 'vivid-colors.dispatch::vprint-dispatch)
		(= 1 (hash-table-count (vivid-colors.dispatch::vprint-dispatch-table result)))
		(equalp (gethash 'null (vivid-colors.dispatch::vprint-dispatch-table result))
			(vivid-colors.dispatch::make-vprinter :type 'null
							      :function 'car))))

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about COPY-VPRINT-DISPATCH :doc-type function
		    :around (let ((*vprint-dispatch*
				    (vivid-colors.dispatch::make-vprint-dispatch
				      :name :dummy)))
			      (call-body)))

;;;; Description:

#+syntax (COPY-VPRINT-DISPATCH &optional (vprint-dispatch *vprint-dispatch*))
; => result

;;;; Arguments and Values:
; COPY-PPRINT-DISPATCH for VPRINT-STREAM.

; vprint-dispatch := unspecified.
; If not specified, copy the value of *VPRINT-DISPATCH*.
#?(let ((copy (copy-vprint-dispatch)))
    (values copy
	    (eq copy *vprint-dispatch*)))
:multiple-value-satisfies (lambda (copy eq?)
			    (& (equalp copy (vivid-colors.dispatch::make-vprint-dispatch
					      :name :dummy))
			       (not eq?)))

; If specified NIL, default vprint-dispatch table is copied.
#?(let ((vivid-colors.dispatch::*vprint-dispatch-repository* (make-hash-table)))
    (store-vprint-dispatch :standard (vivid-colors.dispatch::make-vprint-dispatch
				       :name :standard))
    (let ((*vprint-dispatch* (vivid-colors.dispatch::make-vprint-dispatch :name :dummy)))
      (values (equalp *vprint-dispatch* (copy-vprint-dispatch nil))
	      (equalp (find-vprint-dispatch :standard)
		      (copy-vprint-dispatch nil)))))
:values (NIL T)

; If vprint-dispatch is specified, such vprint-dispatch is copied.
#?(let ((table (vivid-colors.dispatch::make-vprint-dispatch
		 :name :dummy
		 :table (alexandria:plist-hash-table
			  (list 'null (vivid-colors.dispatch::make-vprinter :type 'null :function 'car))))))
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
#?(store-vprint-dispatch "not symbol" (vivid-colors.dispatch::make-vprint-dispatch :name :dummy))
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
#?(merge-vprint-dispatch (vivid-colors.dispatch::make-vprint-dispatch :name :dummy)
			 "not vprint-dispatch")
:signals condition

; result := vprint-dispatch

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:
; Always newly allocated vprint-dispatch is returned.
#?(let ((vprint-dispatch (vivid-colors.dispatch::make-vprint-dispatch :name :dummy)))
    (eq vprint-dispatch (merge-vprint-dispatch vprint-dispatch)))
=> NIL

;;;; Exceptional-Situations:
; When same key-type is exists, an error is signaled.
#?(let ((vivid-colors.dispatch::*vprint-dispatch-repository* (make-hash-table)))
    (store-vprint-dispatch :standard (vivid-colors.dispatch::make-vprint-dispatch
				       :name :standard
				       :table
				       (alexandria:plist-hash-table
					 (list 'null (vivid-colors.dispatch::make-vprinter
						       :type 'null
						       :function 'list)))))
    (let ((*vprint-dispatch* (vivid-colors.dispatch::make-vprint-dispatch
			       :name :dummy
			       :table (alexandria:plist-hash-table
					(list 'null (vivid-colors.dispatch::make-vprinter
						      :type 'null
						      :function 'list))))))
      (merge-vprint-dispatch *vprint-dispatch* (find-vprint-dispatch :standard))))
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
#?(let ((vivid-colors.dispatch::*vprint-dispatch-repository* (make-hash-table)))
    (values (hash-table-count vivid-colors.dispatch::*vprint-dispatch-repository*)
	    (progn (define-vprint-dispatch :dummy)
		   (hash-table-count vivid-colors.dispatch::*vprint-dispatch-repository*))))
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
#?(let ((vivid-colors.dispatch::*vprint-dispatch-repository*
	  (make-hash-table)))
    (store-vprint-dispatch :first (vivid-colors.dispatch::make-vprint-dispatch :name :first))
    (let ((*vprint-dispatch*
	    (vivid-colors.dispatch::make-vprint-dispatch :name :second)))
      (values (vivid-colors.dispatch::vprint-dispatch-name *vprint-dispatch*)
	      (progn (in-vprint-dispatch :first)
		     (vivid-colors.dispatch::vprint-dispatch-name *vprint-dispatch*)))))
:values (:SECOND :FIRST)

;;;; Notes:

;;;; Exceptional-Situations:

