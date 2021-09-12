(defpackage :vivid-colors.content.spec
  (:import-from :vivid-colors.content
		#:compute-length #:mandatory? #:print-content #:with-print-context)
  (:use :cl :jingoh :vivid-colors.content))
(in-package :vivid-colors.content.spec)
(setup :vivid-colors.content)

(requirements-about MAKE-SECTION :doc-type function)

;;;; Description:

#+syntax (MAKE-SECTION &key
           ((:start #:start) 0)
           ((:prefix #:prefix) "")
           ((:contents #:contents) (vivid-colors.queue:new :type 'content))
           ((:suffix #:suffix) ""))
; => result

;;;; Arguments and Values:

; start := (integer 0 #.most-positive-fixnum), otherwise implementation dependent condition.
#?(make-section :start "not integer") :signals condition
#?(make-section :start -1) :signals condition

; prefix := string, otherwise implementation dependent condition.
#?(make-section :prefix :not-string) :signals condition

; contents := vivid-colors.queue:queue, otherwise implementation dependent condition.
#?(make-section :contents "not queue") :signals condition

; suffix := string, otherwise implementation dependent condition.
#?(make-section :suffix :not-string) :signals condition

; result := section object.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MAKE-OBJECT :doc-type function)

;;;; Description:

#+syntax (MAKE-OBJECT &key
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
#?(make-object :content :dummy :color '("not color")) :signals condition
#?(make-object :content :dummy :color "not list") :signals condition
#?(make-object :content :dummy :color (list cl-colors2:+red+ :unknown-key :dummy)) :signals condition
#?(make-object :content :dummy :color (list cl-colors2:+red+ :effect :unknown-effect)) :signals condition
#?(make-object :content :dummy :color (list cl-colors2:+red+ :style :unknown-style)) :signals condition

; key := function, otherwise implementation dependent condition
#?(make-object :content :dummy :key 'prin1-to-string) :signals condition

; result := object

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MAKE-COLORED-STRING :doc-type function)

;;;; Description:

#+syntax (MAKE-COLORED-STRING &key ((:spec #:spec) nil)) ; => result

;;;; Arguments and Values:

; spec := list, otherwise implementation dependent condition.
#?(make-colored-string :spec "not list") :signals condition
; each elt must [ string | cl-ansi-text:color-specifier | spec ]
; spec := (cons string (cons cl-ansi-text:color-specifier args))
; args := {[ :effect effect | :style style ]}*
; effect, for details, evaluate (mapcar #'car cl-ansi-text::+term-effects+).
; style, for details, evaluates (mapcar #'car cl-ansi-text::+text-style+).
; otherwise implementation-dependent-condition.
#?(make-colored-string :spec '(#:not-string-nor-color-specifier)) :signals condition
#?(make-colored-string :spec '(())) :signals condition
#?(make-colored-string :spec '((#:not-string))) :signals condition
#?(make-colored-string :spec '(("dummy" #:not-color-specifier))) :signals condition
#?(make-colored-string :spec '(("dummy" #.cl-colors2:+red+ :unknown-key :dummy))) :signals condition
#?(make-colored-string :spec '(("dummy" #.cl-colors2:+red+ :effect :unknown))) :signals condition
#?(make-colored-string :spec '(("dummy" #.cl-colors2:+red+ :style :unknown))) :signals condition

; result := colord-string

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MAKE-INDENT :doc-type function)

;;;; Description:

#+syntax (MAKE-INDENT &key ((:kind #:kind) :block) ((:width #:width) 0))
; => result

;;;; Arguments and Values:

; kind := [ :block | :current ], otherwise implementation dependent condition.
#?(make-indent :kind :unknown) :signals condition

; width := (integer 0 #.most-positive-fixnum), otherwise implementation dependent condition.
#?(make-indent :width "not integer") :signals condition
#?(make-indent :width -1) :signals condition

; result := indent

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MAKE-NEWLINE :doc-type function)

;;;; Description:

#+syntax (MAKE-NEWLINE &key ((:kind #:kind) (error "kind is required.")))
; => result

;;;; Arguments and Values:

; kind := newline-kind, otherwise implementation dependent condition.
#?(make-newline :kind :unknown) :signals condition

; result := newline

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADD-CONTENT :doc-type function)

;;;; Description:

#+syntax (ADD-CONTENT object section) ; => result

;;;; Arguments and Values:

; object := content, otherwise implementation dependent condition.
#?(add-content "not content" (make-section)) :signals condition

; section := section, otherwise implementation dependent condition.
#?(add-content #\space "not section") :signals condition

; result := object

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify SECTION.
#?(let ((section (make-section)))
    (values (vivid-colors.content::contents-list section)
	    (add-content #\space section)
	    (vivid-colors.content::contents-list section)))
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

(requirements-about COMPUTE-LENGTH :doc-type function)

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
    (compute-length (make-object :content t)))
:invokes-debugger not
#?(let ((*print-circle* t))
    (compute-length (make-object :content t)))
:signals program-error

;;;; Tests.
; Case character.
#?(compute-length #\a) => 1

; Case indent.
#?(compute-length (make-indent)) => 0

; Case newline.
#?(compute-length (make-newline :kind :linear)) => 0

; Case object without color.
#?(compute-length (make-object :content t)) => 1

; Case object with color.
; Color escape sequence length must be ignored even if *print-vivid* is true.
#?(let ((*print-vivid* t))
    (compute-length (make-object :content t :color (list cl-colors2:+red+))))
=> 1

; Case object with key function.
#?(compute-length (make-object :content t :key (lambda (x) (format nil ":~A" x)))) => 2

; Corner case: Is delimiter included?
#?(compute-length (make-object :content "")) => 2

; Case if object is shared and *print-circle* is true, #n= should be printed before contents notation.
#?(vivid-colors.shared:context ()
    (let ((string "dummy")
	  (*print-circle* t))
      (vivid-colors.shared:store string)
      (vivid-colors.shared:store string)
      (vivid-colors.shared:with-check-object-seen ()
        (compute-length (make-object :content string)))))
:equivalents (+ 3 ; #n=
		(length (prin1-to-string "dummy")))

; When *print-cirlce* is NIL.
#?(vivid-colors.shared:context ()
    (let ((string "dummy")
	  (*print-circle*)) ; <--- NOTE!
      (vivid-colors.shared:store string)
      (vivid-colors.shared:store string)
      (vivid-colors.shared:with-check-object-seen ()
        (compute-length (make-object :content string)))))
:equivalents (length (prin1-to-string "dummy"))

; Case if object is shared and already printed and *print-circle* is true, #n# is printed.
#?(vivid-colors.shared:context ()
    (let ((string "dummy")
	  (*print-circle* t))
      (vivid-colors.shared:store string)
      (vivid-colors.shared:store string)
      (vivid-colors.shared:with-check-object-seen ()
        (vivid-colors.shared:mark-printed string)
        (compute-length (make-object :content string)))))
=> 3

#?(vivid-colors.shared:context ()
    (let ((string "dummy")
	  (*print-circle*)) ; <--- NOTE!
      (vivid-colors.shared:store string)
      (vivid-colors.shared:store string)
      (vivid-colors.shared:with-check-object-seen ()
        (vivid-colors.shared:mark-printed string)
        (compute-length (make-object :content string)))))
:equivalents (length (prin1-to-string "dummy"))

; Case colored-string.
; Color escape sequence length must be ignored even if *print-vivid* is true.
#?(let ((*print-vivid* t))
    (compute-length (make-colored-string :spec (list "one" (list "two" cl-colors2:+red+) "three"))))
:equivalents (length (prin1-to-string "onetwothree"))

; Case section.
; When empty section.
#?(compute-length (make-section)) => 0

; With prefix.
#?(compute-length (make-section :prefix "(")) => 1

; With suffix.
#?(compute-length (make-section :suffix ")")) => 1

; With contents character.
#?(let ((s (make-section)))
    (add-content #\a s)
    (compute-length s))
=> 1

; With contents some characters.
#?(let ((s (make-section)))
    (add-content #\a s)
    (add-content #\a s)
    (compute-length s))
=> 2

; With nested section.
#?(let ((s (make-section :prefix "(" :suffix ")")))
    (add-content (make-section :prefix "#(" :suffix ")")  s)
    (compute-length s))
=> 5

(requirements-about MANDATORY? :doc-type function)

;;;; Description:
; Evaluated to be true, if :mandatory newline is found.

#+syntax (MANDATORY? section) ; => result

;;;; Arguments and Values:

; section := section, otherwise implementation dependent condition.
#?(mandatory? "not section") :signals condition

; result := boolean.
#?(mandatory? (make-section)) => nil
#?(let ((section (make-section)))
    (add-content (make-newline :kind :mandatory) section)
    (mandatory? section))
=> T

; Case nested.
#?(let ((outer (make-section))
	(inner (make-section)))
    (add-content (make-newline :kind :mandatory) inner)
    (add-content inner outer)
    (mandatory? outer))
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

(requirements-about PRINT-CONTENT :doc-type function)

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
#?(let ((section (make-section)))
    (add-content (make-newline :kind :mandatory) section)
    (with-print-context (:pretty t)
      (print-content section *standard-output*)))
:outputs "
"

#?(let ((section (make-section)))
    (add-content (make-newline :kind :mandatory) section)
    (with-print-context (:pretty nil) ; <--- NOTE!
      (print-content section *standard-output*)))
:outputs ""

; Case :linear newline.
; If line over *print-right-margin*, newline is printed.
#?(let ((section (make-section)))
    (add-content (make-object :content #\a) section)
    (add-content (make-newline :kind :linear) section)
    (add-content (make-object :content #\b) section)
    (with-print-context (:pretty t :right-margin 1)
      (print-content section *standard-output*)))
:outputs "#\\a
#\\b"

#?(let ((section (make-section)))
    (add-content (make-object :content #\a) section)
    (add-content (make-newline :kind :linear) section)
    (add-content (make-object :content #\b) section)
    (with-print-context (:pretty nil ; <--- NOTE!
                         :right-margin 1)
      (print-content section *standard-output*)))
:outputs "#\\a#\\b"

#?(let ((section (make-section)))
    (add-content (make-object :content #\a) section)
    (add-content (make-newline :kind :linear) section)
    (add-content (make-object :content #\b) section)
    (with-print-context (:pretty t :right-margin 80)
      (print-content section *standard-output*)))
:outputs "#\\a#\\b"

#?(let ((section (make-section)))
    (add-content (make-object :content #\a) section)
    (add-content (make-newline :kind :linear) section)
    (add-content (make-object :content #\b) section)
    (with-print-context (:pretty t :right-margin nil)
      (print-content section *standard-output*)))
:outputs "#\\a#\\b"

; Case :miser newline.
; if and only if the immediately containing section cannot be printed on one line and
; miser style is in effect in the immediately containing logical block. 
; Miser style is in effect for a logical block if and only if the starting position of
; the logical block is less than or equal to *print-miser-width* ems from the right margin. 
#?(let ((section (make-section)))
    (add-content (make-object :content #\a) section)
    (add-content (make-newline :kind :miser) section)
    (add-content (make-object :content #\b) section)
    (with-print-context (:pretty t :right-margin 5 :miser-width 5)
      (print-content section *standard-output*)))
:outputs "#\\a
#\\b"

#?(let ((section (make-section)))
    (add-content (make-object :content #\a) section)
    (add-content (make-newline :kind :miser) section)
    (add-content (make-object :content #\b) section)
    (with-print-context (:pretty nil :right-margin 5 :miser-width 3)
      (print-content section *standard-output*)))
:outputs "#\\a#\\b"

#?(let ((section (make-section)))
    (add-content (make-object :content #\a) section)
    (add-content (make-newline :kind :miser) section)
    (add-content (make-object :content #\b) section)
    (with-print-context (:pretty t :right-margin 10 :miser-width 3)
      (print-content section *standard-output*)))
:outputs "#\\a#\\b"

; Case :fill newline.
; (a) the following section cannot be printed on the end of the current line
#?(let ((section (make-section)))
    (add-content (make-object :content #\a) section)
    (add-content (make-newline :kind :fill) section)
    (add-content (make-object :content #\b) section)
    (with-print-context (:pretty t :right-margin 5)
      (print-content section *standard-output*)))
:outputs "#\\a
#\\b"

; (b) the preceding section was not printed on a single line
; (c) the immediately containing section cannot be printed on one line and 
; miser style is in effect in the immediately containing logical block.

; Case indent.
#?(let ((section (make-section :prefix "(")))
    (add-content (make-object :content #\a) section)
    (add-content (make-newline :kind :mandatory) section)
    (add-content (make-object :content #\b) section)
    (with-print-context (:pretty t)
      (print-content section *standard-output*)))
:outputs "(#\\a
 #\\b"

#?(let ((section (make-section :prefix "(")))
    (add-content (make-object :content #\a) section)
    (add-content (make-indent :width 1) section)
    (add-content (make-newline :kind :mandatory) section)
    (add-content (make-object :content #\b) section)
    (with-print-context (:pretty t)
      (print-content section *standard-output*)))
:outputs "(#\\a
  #\\b"

#?(let ((section (make-section :prefix "(")))
    (add-content (make-object :content #\a) section)
    (add-content (make-indent :kind :current) section)
    (add-content (make-newline :kind :mandatory) section)
    (add-content (make-object :content #\b) section)
    (with-print-context (:pretty t)
      (print-content section *standard-output*)))
:outputs "(#\\a
    #\\b"

(requirements-about WRITE-CONTENT :doc-type function)

;;;; Description:

#+syntax (WRITE-CONTENT content
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
#?(write-content "not content") :signals condition

; stream := stream, otherwise implementation dependent condition.
#?(write-content #\a :stream "not stream") :signals condition

; *print-vivid* := boolean

; *print-circle* := boolean

; *print-pretty* := boolean

; *print-right-margin* := (or null (mod 4611686018427387901)), otherwise implementation dependent condition.
#?(write-content #\a :right-margin "not (or null unsigned-byte)") :signals condition

; *print-miser-width* := (or null (mod 4611686018427387901))
#?(write-content #\a :miser-width "not (or null unsigned-byte)") :signals condition

; result := t

;;;; Affected By:
; none

;;;; Side-Effects:
; Put to stream.

;;;; Notes:

;;;; Exceptional-Situations:

