; vim: ft=lisp et
(in-package :asdf)
(defsystem "vivid-colors.shared.test"
  :version
  "0.1.1"
  :depends-on
  (:jingoh "vivid-colors.shared")
  :components
  ((:file "vivid-colors.shared"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :vivid-colors.shared args)))
