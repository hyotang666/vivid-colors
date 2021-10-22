(defpackage :vivid-colors.stream.spec
  (:use :cl :jingoh :vivid-colors.stream))
(in-package :vivid-colors.stream.spec)
(setup :vivid-colors.stream)

(requirements-about PUT-STRINGS :doc-type function)

;;;; Description:
; Similar with WRITE-STRING but for partially colored string.

#+syntax (PUT-STRINGS strings output) ; => result

;;;; Arguments and Values:

; strings := list, otherwise implementation dependent condition.
#?(put-strings "not list" (make-instance 'vivid-colors.stream::vprint-stream))
:signals condition
; List element must be a string-specifier,
; string-specifier := [ string | (cons string (cons cl-ansi-text:color-specifier (cons args))) ]
; otherwise an error is signaled.
#?(put-strings '(not string specifier) (make-instance 'vivid-colors.stream::vprint-stream))
:signals error
#?(put-strings '((not-string #.cl-colors2:+red+)) (make-instance 'vivid-colors.stream::vprint-stream))
:signals error
#?(put-strings '(("string" not-color-specifier)) (make-instance 'vivid-colors.stream::vprint-stream))
:signals error

; args := [ :effect effect | :style style ]
; To see details of effect eval (mapcar #'car cl-ansi-text::+term-effects+).
; To see details of style eval (mapcar #'car cl-ansi-text::+text-style+).
#?(put-strings '(("string" #.cl-colors2:+red+ :unknown-key 0)) (make-instance 'vivid-colors.stream::vprint-stream))
:signals error
#?(put-strings '(("string" #.cl-colors2:+red+ :effect :no-such-effect)) (make-instance 'vivid-colors.stream::vprint-stream))
:signals error
#?(put-strings '(("string" #.cl-colors2:+red+ :style :no-such-style)) (make-instance 'vivid-colors.stream::vprint-stream))
:signals error

; output := vprint-stream, otherwise implementation dependent condition.
#?(put-strings '("dummy") "not vprint stream") :signals condition

; result := null

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify VPRINT-STREAM state.
#?(vivid-colors.shared:context ()
    (let ((vs (make-instance 'vivid-colors.stream::vprint-stream)))
      (put-strings '("1" ("2" #.cl-colors2:+red+)) vs)
      (vivid-colors.queue:contents (vivid-colors.content::contents (vivid-colors.stream::section vs)))))
:satisfies (lambda (list)
             (& (equalp list
                        (list (vivid-colors.content:colored-string
                                :spec '("1" ("2" #.cl-colors2:+red+)))))))

;;;; Notes:
; Acutually printing is not done.

;;;; Exceptional-Situations:

(requirements-about PUT :doc-type function)

;;;; Description:
; WRITE for VPRINT-STREAM.

#+syntax (PUT object output &key color (key #'prin1-to-string)) ; => result

;;;; Arguments and Values:

; object := t

; output := vprint-stream, otherwise implementation dependent condition.
#?(put t "not vprint stream") :signals condition

; color := (or null cl-ansi-text:color-specifier (cons cl-ansi-text:color-specifier args), otherwise implementation dependent condition.
#?(put t (make-instance 'vivid-colors.stream::vprint-stream) :color "not color specifier")
:signals condition
#?(put t (make-instance 'vivid-colors.stream::vprint-stream) :color (list cl-colors2:+red+ :unknown-key :dummy))
:signals condition
#?(put t (make-instance 'vivid-colors.stream::vprint-stream) :color (list cl-colors2:+red+ :effect :unknown-effect))
:signals condition
#?(put t (make-instance 'vivid-colors.stream::vprint-stream) :color (list cl-colors2:+red+ :style :unknown-style))
:signals condition
#?(vivid-colors.shared:context ()
    (put t (make-instance 'vivid-colors.stream::vprint-stream)
	 :color nil))
=> T

; key := (or symbol function), otherwise implementation dependent condition.
#?(put t (make-instance 'vivid-colors.stream::vprint-stream) :key "not function designator")
:signals condition
; KEY is as (function (object) string), if return type is not string, an error is signaled.
#?(let ((vs(make-instance 'vivid-colors.stream::vprint-stream)))
    (put t vs :key #'identity)
    (finish-output vs))
:signals error

; result := object

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify VPRINT-STREAM state.
#?(vivid-colors.shared:context ()
    (let ((vs (make-instance 'vivid-colors.stream::vprint-stream)))
      (values (copy-list (vivid-colors.queue:contents (vivid-colors.content::contents (vivid-colors.stream::section vs))))
	      (put #\a vs)
	      (vivid-colors.queue:contents (vivid-colors.content::contents (vivid-colors.stream::section vs))))))
:multiple-value-satisfies
(lambda (before put after)
  (& (null before)
     (eql #\a put)
     (equalp after (list (vivid-colors.shared:context ()
                           (vivid-colors.content:object
                             :content #\a))))))

;;;; Notes:
; Because above side effect, you should use PUT rather than WRITE for VPRINT-STREAM.

;;;; Exceptional-Situations:
; If dynamic context is not achieved, an error is signaled.
#?(put t (make-instance 'vivid-colors.stream::vprint-stream)) :signals program-error

(requirements-about VPRINT-NEWLINE :doc-type function
                    :around (let ((*print-pretty* t))
                              (call-body)))

;;;; Description:

#+syntax (VPRINT-NEWLINE kind output) ; => result

;;;; Arguments and Values:

; kind := (member :miser :fill :linear :mandatory), otherwise implementation dependent condition.
#?(vivid-colors.shared:context ()
    (vprint-newline "not member" (make-instance 'vivid-colors.stream::vprint-stream)))
:signals condition

; output := vprint-stream, otherwise implementation dependent condition.
#?(vprint-newline :linear "not vprint stream") :signals condition

; result := (VALUES)

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify VPRINT-STREAM state.
#?(vivid-colors.shared:context ()
    (let ((vs (make-instance 'vivid-colors.stream::vprint-stream)))
      (vprint-newline :linear vs)
      (vivid-colors.queue:contents (vivid-colors.content::contents (vivid-colors.stream::section vs)))))
:satisfies (lambda (list)
             (& (equalp list
                        (list (vivid-colors.content:newline :kind :linear)))))

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about VPRINT-INDENT :doc-type function)

;;;; Description:

#+syntax (VPRINT-INDENT kind indent output) ; => result

;;;; Arguments and Values:

; kind := (member :block :current), otherwise implementation dependent condition.
#?(vivid-colors.shared:context ()
    (vprint-indent "not member" 0 (make-instance 'vivid-colors.stream::vprint-stream)))
:signals condition

; indent := (unsigned-byte 62), otherwise implementation dependent condition.
#?(vprint-indent :block "not unsigned-byte" (make-instance 'vivid-colors.stream::vprint-stream))
:signals condition

; output := vprint-stream, otherwise implementation dependent condition.
#?(vprint-indent :blocl 0 "not vprint stream") :signals condition

; result := (values)

;;;; Affected By:
; none

;;;; Side-Effects:
; Modify VPRINT-STREAM state.
#?(vivid-colors.shared:context ()
    (let ((vs (make-instance 'vivid-colors.stream::vprint-stream)))
      (values (vivid-colors.queue:contents (vivid-colors.content::contents (vivid-colors.stream::section vs)))
              (progn (vprint-indent :block 3 vs)
                     (vivid-colors.queue:contents (vivid-colors.content::contents (vivid-colors.stream::section vs)))))))
:multiple-value-satisfies
(lambda (before after)
  (& (null before)
     (equalp after
             (list (vivid-colors.content:indent :width 3)))))

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
; vivid-colors.stream::*vstream*
; If *VSTREAM* is unbound, VPRINT-STREAM is newly allocated.
#?(values (boundp 'vivid-colors.stream::*vstream*)
          (vprint-logical-block (nil nil)
            (boundp 'vivid-colors.stream::*vstream*)))
:values (nil t)

; If *VSTREAM* is already bound, its section is modified.
#?(vivid-colors.shared:context ()
    (let* ((vs (make-instance 'vivid-colors.stream::vprint-stream))
           (vivid-colors.stream::*vstream* vs)
           (sec (vivid-colors.stream::section vivid-colors.stream::*vstream*)))
      (vprint-logical-block (nil nil)
        (values (eq vs vivid-colors.stream::*vstream*)
                (eq sec (vivid-colors.stream::section vivid-colors.stream::*vstream*))))))
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
#?(let ((vivid-colors.dispatch::*vprint-dispatch-repository* (make-hash-table)))
    (vivid-colors.dispatch::store-vprint-dispatch
      :standard

      (vivid-colors.dispatch::make-vprint-dispatch
	:name :standard))
    (let ((vivid-colors.dispatch:*vprint-dispatch*
	   (vivid-colors.dispatch:find-vprint-dispatch :standard)))
      (vprint-logical-block (nil 'not-list))))
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

