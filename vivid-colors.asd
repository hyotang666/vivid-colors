; vim: ft=lisp et
(in-package :asdf)
(defsystem "vivid-colors"
  :version
  "0.18.3"
  :depends-on
  (
   "trivial-gray-streams"       ; Wrapper for gray-streams.
   "cl-ansi-text"               ; ANSI color control sequence.
   "cl-colors2"                 ; Color objects. Implicitly depends on via cl-ansi-text.
   "uiop"                       ; Utilities. Implicitly depends on via asdf.
   )
  :pathname
  "src/"
  :components
  ((:file "vivid-colors")))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "vivid-colors").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "vivid-colors"))))
  (append (call-next-method) '((test-op "vivid-colors.test"))))
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