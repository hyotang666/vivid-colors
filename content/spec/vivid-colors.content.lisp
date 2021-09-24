(defpackage :vivid-colors.content.spec
  (:import-from :vivid-colors.content
		#:compute-length #:mandatory? #:print-content #:with-print-context)
  (:use :cl :jingoh :vivid-colors.content))
(in-package :vivid-colors.content.spec)
(setup :vivid-colors.content)

(requirements-about SECTION :doc-type function)

;;;; Description:

#+syntax (SECTION &key
           (start 0)
           (prefix "")
           (contents (vivid-colors.queue:new :type 'content))
           (suffix "")
           (color *color*)
           expression)
; => result

;;;; Arguments and Values:

; start := (integer 0 #.most-positive-fixnum), otherwise implementation dependent condition.
#?(vivid-colors.shared:context ()
    (section :start "not integer"))
:signals condition
#?(vivid-colors.shared:context ()
    (section :start -1))
:signals condition

; prefix := string, otherwise implementation dependent condition.
#?(vivid-colors.shared:context ()
    (section :prefix :not-string))
:signals condition

; contents := vivid-colors.queue:queue, otherwise implementation dependent condition.
#?(vivid-colors.shared:context ()
    (section :contents "not queue"))
:signals condition

; suffix := string, otherwise implementation dependent condition.
#?(vivid-colors.shared:context ()
    (section :suffix :not-string))
:signals condition

; color := (or null color-spec)
#?(vivid-colors.shared:context ()
    (section :color "not color spec"))
:signals condition

; expression := t

; result := section object.

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify vivid-colors.shared::*shared-objects* when expression satisfies vivid-colors.shared:should-do-p.
#?(vivid-colors.shared:context ()
    (values (hash-table-count vivid-colors.shared::*shared-objects*)
	    (progn (section :expression '(1 2 3))
		   (hash-table-count vivid-colors.shared::*shared-objects*))))
:values (0 1)

;;;; Notes:

;;;; Exceptional-Situations:
; If context is not achieved, an error is signaled.
#?(section) :signals program-error

(requirements-about OBJECT :doc-type function)

;;;; Description:

#+syntax (OBJECT &key
           ((:content #:content) (error "content is required."))
           ((:color #:color) nil)
           ((:key #:key) #'prin1-to-string))
; => result

;;;; Arguments and Values:

; content := t

; color := (or null (cons cl-ansi-text:color-specifier args))
; args := {[ :effect effect | :style style ]}*
; effect, for details, evaluate (mapcar #'car cl-ansi-text::+term-effects+).
; style, for details, evaluates (mapcar #'car cl-ansi-text::+text-style+).
; otherwise implementation-dependent-condition.
#?(vivid-colors.shared:context ()
    (object :content :dummy :color '("not color")))
:signals condition
#?(vivid-colors.shared:context ()
    (object :content :dummy :color "not list"))
:signals condition
#?(vivid-colors.shared:context ()
    (object :content :dummy :color (list cl-colors2:+red+ :unknown-key :dummy)))
:signals condition
#?(vivid-colors.shared:context ()
    (object :content :dummy :color (list cl-colors2:+red+ :effect :unknown-effect)))
:signals condition
#?(vivid-colors.shared:context ()
    (object :content :dummy :color (list cl-colors2:+red+ :style :unknown-style)))
:signals condition

; key := function, otherwise implementation dependent condition
#?(vivid-colors.shared:context ()
    (object :content :dummy :key 'prin1-to-string))
:signals condition

; result := object

;;;; Affected By:
; none

;;;; Side-Effects:
; vivid-colors.shared::*shared-objects*
#?(vivid-colors.shared:context ()
    (values (hash-table-count vivid-colors.shared::*shared-objects*)
	    (progn (object :content "dummy")
		   (hash-table-count vivid-colors.shared::*shared-objects*))))
:values (0 1)

;;;; Notes:

;;;; Exceptional-Situations:
; When context is not achieved, an error is signaled.
#?(object :content t) :signals program-error

(requirements-about COLORED-STRING :doc-type function)

;;;; Description:

#+syntax (COLORED-STRING &key ((:spec #:spec) nil)) ; => result

;;;; Arguments and Values:

; spec := list, otherwise implementation dependent condition.
#?(colored-string :spec "not list") :signals condition
; each elt must [ string | cl-ansi-text:color-specifier | spec ]
; spec := (cons string (cons cl-ansi-text:color-specifier args))
; args := {[ :effect effect | :style style ]}*
; effect, for details, evaluate (mapcar #'car cl-ansi-text::+term-effects+).
; style, for details, evaluates (mapcar #'car cl-ansi-text::+text-style+).
; otherwise implementation-dependent-condition.
#?(colored-string :spec '(#:not-string-nor-color-specifier)) :signals condition
#?(colored-string :spec '(())) :signals condition
#?(colored-string :spec '((#:not-string))) :signals condition
#?(colored-string :spec '(("dummy" #:not-color-specifier))) :signals condition
#?(colored-string :spec '(("dummy" #.cl-colors2:+red+ :unknown-key :dummy))) :signals condition
#?(colored-string :spec '(("dummy" #.cl-colors2:+red+ :effect :unknown))) :signals condition
#?(colored-string :spec '(("dummy" #.cl-colors2:+red+ :style :unknown))) :signals condition

; result := colord-string

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about INDENT :doc-type function)

;;;; Description:

#+syntax (INDENT &key ((:kind #:kind) :block) ((:width #:width) 0))
; => result

;;;; Arguments and Values:

; kind := [ :block | :current ], otherwise implementation dependent condition.
#?(indent :kind :unknown) :signals condition

; width := (integer 0 #.most-positive-fixnum), otherwise implementation dependent condition.
#?(indent :width "not integer") :signals condition
#?(indent :width -1) :signals condition

; result := indent

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about NEWLINE :doc-type function)

;;;; Description:

#+syntax (NEWLINE &key ((:kind #:kind) (error "kind is required.")))
; => result

;;;; Arguments and Values:

; kind := newline-kind, otherwise implementation dependent condition.
#?(newline :kind :unknown) :signals condition

; result := newline

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about appoint-to-write :doc-type function)

;;;; Description:

#+syntax (appoint-to-write object section) ; => result

;;;; Arguments and Values:

; object := content, otherwise implementation dependent condition.
#?(vivid-colors.shared:context () (appoint-to-write "not content" (section))) :signals condition

; section := section, otherwise implementation dependent condition.
#?(appoint-to-write #\space "not section") :signals condition

; result := object

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify SECTION.
#?(vivid-colors.shared:context ()
    (let ((section (section)))
      (values (vivid-colors.queue:contents (vivid-colors.content::contents section))
              (appoint-to-write #\space section)
              (vivid-colors.queue:contents (vivid-colors.content::contents section)))))
:multiple-value-satisfies
(lambda (first second third)
  (& (null first)
     (eql #\space second)
     (equalp third (list #\space))))

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about NEWLINE-KIND :doc-type type)

;;;; Description:

#+syntax
(newline-kind)

(requirements-about COMPUTE-LENGTH :doc-type function
		    :around (let ((*print-circle* nil)) (call-body)))

;;;; Description:
; Evaluated to be length of THING in if printed one line.

#+syntax (COMPUTE-LENGTH thing) ; => result

;;;; Argument Precedence Order:
; thing

;;;; Method signature:
#+signature(COMPUTE-LENGTH (SECTION SECTION))
#+signature(COMPUTE-LENGTH (S COLORED-STRING))
#+signature(COMPUTE-LENGTH (OBJECT OBJECT))
#+signature(COMPUTE-LENGTH (N NEWLINE))
#+signature(COMPUTE-LENGTH (I INDENT))
#+signature(COMPUTE-LENGTH (C CHARACTER))

;;;; Arguments and Values:

; thing := 

; result := 

;;;; Affected By:
; vivid-colors.shared::*seen?*
; vivid-colors.shared:*shared-objects*
; vivid-colors.shared:*shared-count*
; *print-circle*

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; If *print-circle* is true, but context is not achieved, an error is signaled.
#?(let (*print-circle*)
    (compute-length (object :content t)))
:invokes-debugger not
#?(let ((*print-circle* t))
    (compute-length (object :content t)))
:signals program-error

;;;; Tests.
; Case character.
#?(compute-length #\a) => 1

; Case indent.
#?(compute-length (indent)) => 0

; Case newline.
#?(compute-length (newline :kind :linear)) => 0

; Case object without color.
#?(vivid-colors.shared:context ()
    (compute-length (object :content t)))
=> 1

; Case object with color.
; Color escape sequence length must be ignored even if *print-vivid* is true.
#?(let ((*print-vivid* t))
    (vivid-colors.shared:context ()
      (compute-length (object :content t :color (list cl-colors2:+red+)))))
=> 1

; Case object with key function.
#?(vivid-colors.shared:context ()
    (compute-length (object :content t :key (lambda (x) (format nil ":~A" x)))))
=> 2

; Corner case: Is delimiter included?
#?(vivid-colors.shared:context ()
    (compute-length (object :content "")))
=> 2

; Case if object is shared and *print-circle* is true and appear first, #n= should be printed before contents notation.
; | *print-circle* | object-firstp | sharedp |
; | -------------- | ------------- | ------- |
; | t              | t             | t       |
#?(vivid-colors.shared:context ()
    (let* ((string "dummy")
	   (*print-circle* t)
	   (first (object :content string)))
      (object :content string)
      (compute-length first)))
:equivalents (+ 3 ; #n=
		(length (prin1-to-string "dummy")))

; | t             | t              | nil     |
#?(let ((string "dummy")
	(*print-circle* t))
    (vivid-colors.shared:context ()
      (compute-length (object :content string))))
:equivalents (length (prin1-to-string "dummy"))

; | t             | nil            | nil     |
#?(let ((*print-circle* t))
    (vivid-colors.shared:context ()
      (compute-length (object :content 'string))))
:equivalents (length (prin1-to-string 'string))

; | t             | nil            | t       |
#?(let ((string "dummy")
	(*print-circle* t))
    (vivid-colors.shared:context ()
      (object :content string)
      (compute-length (object :content string))))
:equivalents (length "#0#")

; | nil           | t              | t       |
#?(vivid-colors.shared:context ()
    (let* ((string "dummy")
	   (*print-circle* nil)
	   (first (object :content string)))
      (object :content string)
      (compute-length first)))
:equivalents (length (prin1-to-string "dummy"))

; | nil           | t              | nil     |
#?(let ((string "dummy")
	(*print-circle* nil))
    (vivid-colors.shared:context ()
      (compute-length (object :content string))))
:equivalents (length (prin1-to-string "dummy"))

; | nil           | nil            | t       |
#?(let ((string "dummy")
	(*print-circle* nil))
    (vivid-colors.shared:context ()
      (object :content string)
      (compute-length (object :content string))))
:equivalents (length (prin1-to-string "dummy"))

; | nil           | nil            | nil     |
#?(let ((*print-circle* nil))
    (vivid-colors.shared:context ()
      (compute-length (object :content 'string))))
:equivalents (length (prin1-to-string 'string))

; Case colored-string.
; Color escape sequence length must be ignored even if *print-vivid* is true.
#?(let ((*print-vivid* t))
    (compute-length (colored-string :spec (list "one" (list "two" cl-colors2:+red+) "three"))))
:equivalents (length (prin1-to-string "onetwothree"))

; Case section.
; When empty section.
#?(vivid-colors.shared:context () (compute-length (section))) => 0

; With prefix.
#?(vivid-colors.shared:context () (compute-length (section :prefix "("))) => 1

; With suffix.
#?(vivid-colors.shared:context () (compute-length (section :suffix ")"))) => 1

; With contents character.
#?(vivid-colors.shared:context ()
    (let ((s (section)))
      (appoint-to-write #\a s)
      (compute-length s)))
=> 1

; With contents some characters.
#?(vivid-colors.shared:context ()
    (let ((s (section)))
      (appoint-to-write #\a s)
      (appoint-to-write #\a s)
      (compute-length s)))
=> 2

; With nested section.
#?(vivid-colors.shared:context ()
    (let ((s (section :prefix "(" :suffix ")")))
      (appoint-to-write (section :prefix "#(" :suffix ")")  s)
      (compute-length s)))
=> 5

; Case string.
#?(vivid-colors.shared:context ()
    (let ((s (section :prefix "(" :suffix ")"))
          (string ""))
      (appoint-to-write (object :content string) s)
      (vivid-colors.shared:context ()
        (compute-length s))))
:equivalents (length (prin1-to-string '("")))

; Case *print-circle* t.
#?(vivid-colors.shared:context ()
    (let ((*print-circle* t)
	  (s (section :prefix "(" :suffix ")"))
	  (string ""))
      (appoint-to-write (object :content string) s)
      (appoint-to-write #\space s)
      (appoint-to-write (object :content string) s)
      (vivid-colors.shared:id (vivid-colors.shared::storedp string) :if-does-not-exist :set)
      (compute-length s)))
:equivalents (length (write-to-string '(#0="" #0#) :circle t :escape t))

; Case circular list of CAR.
#?(vivid-colors.shared:context ()
    (let* ((*print-circle* t)
	   (content (let ((list (cons nil nil)))
		      (rplaca list list)))
	   (s (section :prefix "(" :suffix ")" :expression content)))
      (vivid-colors.shared:store (car content))
      (appoint-to-write (reference :section s) s)
      (compute-length s)))
:equivalents (length (write-to-string (let ((list (cons nil nil)))
					(rplaca list list))
				      :circle t :escape t))

; Case circluar list of CDR.
#?(vivid-colors.shared:context ()
    (let* ((*print-circle* t)
	   (content (alexandria:circular-list t))
	   (s (section :prefix "(" :suffix ")" :expression content)))
      (vivid-colors.shared:store (cdr content))
      (appoint-to-write (object :content t) s)
      (appoint-to-write (reference :section s) s)
      (compute-length s)))
:equivalents (length (write-to-string (alexandria:circular-list t) :circle t :escape t))

(requirements-about MANDATORY? :doc-type function)

;;;; Description:
; Evaluated to be true, if :mandatory newline is found.

#+syntax (MANDATORY? section) ; => result

;;;; Arguments and Values:

; section := section, otherwise implementation dependent condition.
#?(mandatory? "not section") :signals condition

; result := boolean.
#?(vivid-colors.shared:context () (mandatory? (section))) => nil
#?(vivid-colors.shared:context ()
    (let ((section (section)))
      (appoint-to-write (newline :kind :mandatory) section)
      (mandatory? section)))
=> T

; Case nested.
#?(vivid-colors.shared:context ()
    (let ((outer (section))
          (inner (section)))
      (appoint-to-write (newline :kind :mandatory) inner)
      (appoint-to-write inner outer)
      (mandatory? outer)))
=> T

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about WITH-PRINT-CONTEXT :doc-type function)

;;;; Description:

#+syntax (WITH-PRINT-CONTEXT (&key (vivid '*print-vivid*) (circle '*print-circle*)
  (right-margin '*print-right-margin*) (miser-width '*print-miser-width*))
           &body
           body)
; => result

;;;; Arguments and Values:

; vivid := The form for *print-vivid*.

; circle := The form for *print-circle*.

; right-margin := The form for *print-right-margin*.

; miser-width := The form for *print-miser-width*.

; body := implicit progn.

; result := The return value of the BODY.

;;;; Affected By:
; none.

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about PRINT-CONTENT :doc-type function
		    :around (let ((*print-circle* nil))
			      (call-body)))

;;;; Description:

#+syntax (PRINT-CONTENT content output) ; => result

;;;; Argument Precedence Order:
; content output

;;;; Method signature:
#+signature(PRINT-CONTENT (S SECTION) (O STREAM))
#+signature(PRINT-CONTENT (C COLORED-STRING) (OUTPUT STREAM))
#+signature(PRINT-CONTENT (O OBJECT) (OUTPUT STREAM))
#+signature(PRINT-CONTENT (C CHARACTER) (OUTPUT STREAM))
#+signature(PRINT-CONTENT :AROUND (CONTENT T) (OUTPUT T))

;;;; Arguments and Values:

; content := content

; output := stream

; result := t

;;;; Affected By:
; *print-vivid* *print-circle* *print-right-margin* *print-miser-width*
; *position* *indent* *newlinep*

;;;; Side-Effects:
; Outputs to stream.

;;;; Notes:

;;;; Exceptional-Situations:
; When print-context is not achieved, an error is signaled.
#?(print-content #\a *standard-output*) :signals condition

;;;; Tests.
; Case character.
#?(with-print-context () (print-content #\a *standard-output*))
:outputs "a"

; Case *print-pretty* t, and :mandatory newline.
#?(vivid-colors.shared:context ()
    (let ((section (section)))
      (appoint-to-write (newline :kind :mandatory) section)
      (with-print-context (:pretty t)
        (print-content section *standard-output*))))
:outputs "
"

#?(vivid-colors.shared:context ()
    (let ((section (section)))
      (appoint-to-write (newline :kind :mandatory) section)
      (with-print-context (:pretty nil) ; <--- NOTE!
        (print-content section *standard-output*))))
:outputs ""

; Case :linear newline.
; If line over *print-right-margin*, newline is printed.
#?(vivid-colors.shared:context ()
    (let ((section (section)))
      (appoint-to-write (object :content #\a) section)
      (appoint-to-write (newline :kind :linear) section)
      (appoint-to-write (object :content #\b) section)
      (with-print-context (:pretty t :right-margin 1)
        (print-content section *standard-output*))))
:outputs "#\\a
#\\b"

#?(vivid-colors.shared:context ()
    (let ((section (section)))
      (appoint-to-write (object :content #\a) section)
      (appoint-to-write (newline :kind :linear) section)
      (appoint-to-write (object :content #\b) section)
      (with-print-context (:pretty nil ; <--- NOTE!
                           :right-margin 1)
        (print-content section *standard-output*))))
:outputs "#\\a#\\b"

#?(vivid-colors.shared:context ()
    (let ((section (section)))
      (appoint-to-write (object :content #\a) section)
      (appoint-to-write (newline :kind :linear) section)
      (appoint-to-write (object :content #\b) section)
      (with-print-context (:pretty t :right-margin 80)
        (print-content section *standard-output*))))
:outputs "#\\a#\\b"

#?(vivid-colors.shared:context ()
    (let ((section (section)))
      (appoint-to-write (object :content #\a) section)
      (appoint-to-write (newline :kind :linear) section)
      (appoint-to-write (object :content #\b) section)
      (with-print-context (:pretty t :right-margin nil)
        (print-content section *standard-output*))))
:outputs "#\\a#\\b"

; Case :miser newline.
; if and only if the immediately containing section cannot be printed on one line and
; miser style is in effect in the immediately containing logical block. 
; Miser style is in effect for a logical block if and only if the starting position of
; the logical block is less than or equal to *print-miser-width* ems from the right margin. 
#?(vivid-colors.shared:context ()
    (let ((section (section)))
      (appoint-to-write (object :content #\a) section)
      (appoint-to-write (newline :kind :miser) section)
      (appoint-to-write (object :content #\b) section)
      (with-print-context (:pretty t :right-margin 5 :miser-width 5)
        (print-content section *standard-output*))))
:outputs "#\\a
#\\b"

#?(vivid-colors.shared:context ()
    (let ((section (section)))
      (appoint-to-write (object :content #\a) section)
      (appoint-to-write (newline :kind :miser) section)
      (appoint-to-write (object :content #\b) section)
      (with-print-context (:pretty nil :right-margin 5 :miser-width 3)
        (print-content section *standard-output*))))
:outputs "#\\a#\\b"

#?(vivid-colors.shared:context ()
    (let ((section (section)))
      (appoint-to-write (object :content #\a) section)
      (appoint-to-write (newline :kind :miser) section)
      (appoint-to-write (object :content #\b) section)
      (with-print-context (:pretty t :right-margin 10 :miser-width 3)
        (print-content section *standard-output*))))
:outputs "#\\a#\\b"

; Case :fill newline.
; (a) the following section cannot be printed on the end of the current line
#?(vivid-colors.shared:context ()
    (let ((section (section)))
      (appoint-to-write (object :content #\a) section)
      (appoint-to-write (newline :kind :fill) section)
      (appoint-to-write (object :content #\b) section)
      (with-print-context (:pretty t :right-margin 5)
        (print-content section *standard-output*))))
:outputs "#\\a
#\\b"

; (b) the preceding section was not printed on a single line
; (c) the immediately containing section cannot be printed on one line and 
; miser style is in effect in the immediately containing logical block.

; Case indent.
#?(vivid-colors.shared:context ()
    (let ((section (section :prefix "(")))
      (appoint-to-write (object :content #\a) section)
      (appoint-to-write (newline :kind :mandatory) section)
      (appoint-to-write (object :content #\b) section)
      (with-print-context (:pretty t)
        (print-content section *standard-output*))))
:outputs "(#\\a
 #\\b"

#?(vivid-colors.shared:context ()
    (let ((section (section :prefix "(")))
      (appoint-to-write (object :content #\a) section)
      (appoint-to-write (indent :width 1) section)
      (appoint-to-write (newline :kind :mandatory) section)
      (appoint-to-write (object :content #\b) section)
      (with-print-context (:pretty t)
        (print-content section *standard-output*))))
:outputs "(#\\a
  #\\b"

#?(vivid-colors.shared:context ()
    (let ((section (section :prefix "(")))
      (appoint-to-write (object :content #\a) section)
      (appoint-to-write (indent :kind :current) section)
      (appoint-to-write (newline :kind :mandatory) section)
      (appoint-to-write (object :content #\b) section)
      (with-print-context (:pretty t)
        (print-content section *standard-output*))))
:outputs "(#\\a
    #\\b"

(requirements-about fulfill-to-write :doc-type function)

;;;; Description:

#+syntax (fulfill-to-write content
           &key
           (stream *standard-output*)
           ((:vivid *print-vivid*) *print-vivid*)
           ((:circle *print-circle*) *print-circle*)
           ((:pretty *print-pretty*) *print-pretty*)
           ((:right-margin *print-right-margin*) *print-right-margin*)
           ((:miser-width *print-miser-width*) *print-miser-width*))
; => result

;;;; Arguments and Values:

; content := (or object character indent newline section colored-string), otherwise implementation dependent condition.
#?(fulfill-to-write "not content") :signals condition

; stream := stream, otherwise implementation dependent condition.
#?(fulfill-to-write #\a :stream "not stream") :signals condition

; *print-vivid* := boolean

; *print-circle* := boolean

; *print-pretty* := boolean

; *print-right-margin* := (or null (mod 4611686018427387901)), otherwise implementation dependent condition.
#?(fulfill-to-write #\a :right-margin "not (or null unsigned-byte)") :signals condition

; *print-miser-width* := (or null (mod 4611686018427387901))
#?(fulfill-to-write #\a :miser-width "not (or null unsigned-byte)") :signals condition

; result := t

;;;; Affected By:
; none

;;;; Side-Effects:
; Put to stream.

;;;; Notes:

;;;; Exceptional-Situations:

