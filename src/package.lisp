(in-package :cl-user)

(defpackage :vivid-colors
  (:use :cl)
  (:export ;;;; MAIN API
           "VPRINT" ; like CL:PPRINT
           "*PRINT-VIVID*" ; like CL:*PRINT-PRETTY*
           )
  (:export ;;;; EXTEND
           "SET-VPRINT-DISPATCH" ; like CL:SET-PPRINT-DISPATCH.
           "*VPRINT-DISPATCH*" ; like CL:*PRINT-PPRINT-DISPATCH*.
           "COPY-VPRINT-DISPATCH" ; like CL:COPY-PPRINT-DISPATCH.
           )
  (:export ;;;; EXTEND similar with named-readtables.
           ;;;; MAIN API
           "DEFINE-VPRINT-DISPATCH" ; like CL:DEFPACKAGE.
           "IN-VPRINT-DISPATCH" ; like CL:IN-PACKAGE.
           ;;;; For Hackers.
           "FIND-VPRINT-DISPATCH" ; like CL:FIND-PACKAGE.
           "STORE-VPRINT-DISPATCH" ; like NAMED-READTABLES:REGISTER-READTABLE.
           "MERGE-VPRINT-DISPATCH" ; like
                                   ; NAMED-READTABLES:MERGE-READTABLES-INTO.
           "LIST-ALL-VPRINT-DISPATCHES" ; like CL:LIST-ALL-PACKAGES
           )
  (:export ;;;; HELPERS for color control.
           "PUT" ; like CL:WRITE.
           "PUT-STRINGS" ; for partially colored string.
           )
  (:export ;;;; HELPER for pretty printings.
           "PUT-CHAR" ; like CL:WRITE-CHAR.
           "VPRINT-NEWLINE" ; like CL:PPRINT-NEWLINE.
           "VPRINT-INDENT" ; like CL:PPRINT-INDENT.
           "VPRINT-POP" ; like CL:PPRINT-POP.
           "VPRINT-EXIT-IF-LIST-EXHAUSTED" ; like
                                           ; CL:PPRINT-EXIT-IF-LIST-EXHAUSTED.
           "VPRINT-LOGICAL-BLOCK" ; like CL:PPRINT-LOGICAL-BLOCK.
           ))


