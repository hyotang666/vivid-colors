(in-package :cl-user)

(defpackage :vivid-colors
  (:use :cl)
  (:import-from :vivid-colors.content #:*print-vivid*)
  (:export ;;;; MAIN API
           #:vprint ; like CL:PPRINT
           #:*print-vivid* ; like CL:*PRINT-PRETTY*
           )
  (:export ;;;; EXTEND
           #:set-vprint-dispatch ; like CL:SET-PPRINT-DISPATCH.
           #:*vprint-dispatch* ; like CL:*PRINT-PPRINT-DISPATCH*.
           #:copy-vprint-dispatch ; like CL:COPY-PPRINT-DISPATCH.
           )
  (:export ;;;; EXTEND similar with named-readtables.
           ;;;; MAIN API
           #:define-vprint-dispatch ; like CL:DEFPACKAGE.
           #:in-vprint-dispatch ; like CL:IN-PACKAGE.
           ;;;; For Hackers.
           #:find-vprint-dispatch ; like CL:FIND-PACKAGE.
           #:store-vprint-dispatch ; like NAMED-READTABLES:REGISTER-READTABLE.
           #:merge-vprint-dispatch ; like
                                   ; NAMED-READTABLES:MERGE-READTABLES-INTO.
           #:list-all-vprint-dispatches ; like CL:LIST-ALL-PACKAGES
           )
  (:export ;;;; HELPERS for color control.
           #:put ; like CL:WRITE.
           #:put-strings ; for partially colored string.
           )
  (:export ;;;; HELPER for pretty printings.
           #:vprint-newline ; like CL:PPRINT-NEWLINE.
           #:vprint-indent ; like CL:PPRINT-INDENT.
           #:vprint-pop ; like CL:PPRINT-POP.
           #:vprint-exit-if-list-exhausted ; like
                                           ; CL:PPRINT-EXIT-IF-LIST-EXHAUSTED.
           #:vprint-logical-block ; like CL:PPRINT-LOGICAL-BLOCK.
           ))