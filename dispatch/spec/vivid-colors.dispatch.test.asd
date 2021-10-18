; vim: ft=lisp et
(in-package :asdf)
(defsystem "vivid-colors.dispatch.test"
  :version
  "0.2.1"
  :depends-on
  (:jingoh "vivid-colors.dispatch")
  :components
  ((:file "vivid-colors.dispatch"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :vivid-colors.dispatch args)))
