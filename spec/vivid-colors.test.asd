; vim: ft=lisp et
(in-package :asdf)
(defsystem "vivid-colors.test"
  :version
  "0.3.0"
  :depends-on
  (:jingoh "vivid-colors")
  :components
  ((:file "vivid-colors"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :vivid-colors args)))
