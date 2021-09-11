; vim: ft=lisp et
(in-package :asdf)
(defsystem "vivid-colors.content"
  :version
  "0.0.4"
  :depends-on
  (
   "uiop"                       ; Utilities, implicitly depends on via asdf.
   "mcase"                      ; Syntax, control flow with comprehensiveness checking.
   "cl-ansi-text"               ; Ansi color escape sequence.
   "vivid-colors.shared"        ; Module the shared object.
   )
  :pathname
  "src/"
  :components
  ((:file "content")))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "vivid-colors.content").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "vivid-colors.content"))))
  (append (call-next-method) '((test-op "vivid-colors.content.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "vivid-colors.content")))
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
               ((o load-op) (c (eql (find-system "vivid-colors.content"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (handler-case (symbol-call :jingoh.documentizer :import c)
                      (error (condition)
                             (warn "Fails to import documentation of ~S.~%~A"
                                   (coerce-name c)
                                   (princ-to-string condition))))))))
