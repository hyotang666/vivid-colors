; vim: ft=lisp et
(in-package :asdf)
(defsystem "vivid-colors"
  :version
  "4.7.7"
  :depends-on
  (
   "uiop"                       ; Utilities. Implicitly depends on via asdf.
   "lambda-fiddle"              ; Utilities for lambda list processing.
   "alexandria"                 ; Utilities, implicitly depends on via (cl-colors2 cl-ansi-text).
   "hash-table-ext"             ; Utilities for hash-tables.
   "millet"                     ; Wrapper for implementation dependent utilities.
   "trivial-gray-streams"       ; Wrapper for gray-streams.
   "closer-mop"                 ; Wrapper for Meta Object Protocols.
   "cl-ansi-text"               ; ANSI color control sequence.
   "cl-colors2"                 ; Color objects. Implicitly depends on via cl-ansi-text.
   "vivid-colors.shared"        ; Module the shared object.
   "vivid-colors.content"       ; Module the content objects.
   )
  :pathname
  "src/"
  :components
  ((:file "package")
   (:file "vprint-dispatch" :depends-on ("package"))
   (:file "vivid-colors" :depends-on ("vprint-dispatch")))
  :author "SATO Shinichi"
  :license "MIT"
  :description #.(concatenate 'string
                              (string #\esc)
                              "[31;4mColored"
                              (string #\esc)
                              "[0m "
                              (string #\esc)
                              "[32mobject"
                              (string #\esc)
                              "[0m "
                              (string #\esc)
                              "[34;5mprinter"
                              (string #\esc)
                              "[0m.")
  :source-control (:git "git@github.com:hyotang666/vivid-colors")
  :bug-tracker "https://github.com/hyotang666/vivid-colors/issues")

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "vivid-colors").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "vivid-colors"))))
  (append (call-next-method)
          '((test-op "vivid-colors.queue.test")
            (test-op "vivid-colors.shared.test")
            (test-op "vivid-colors.content.test")
            (test-op "vivid-colors.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "vivid-colors")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when system
    (load-system system)
    (defmethod perform :after
               ((o load-op) (c (eql (find-system "vivid-colors"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (handler-case (symbol-call :jingoh.documentizer :import c)
                      (error (condition)
                             (warn "Fails to import documentation of ~S.~%~A"
                                   (coerce-name c)
                                   (princ-to-string condition))))))))
