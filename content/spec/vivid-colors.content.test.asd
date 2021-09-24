; vim: ft=lisp et
(in-package :asdf)
(defsystem "vivid-colors.content.test"
  :version
  "1.0.0"
  :depends-on
  (:jingoh "vivid-colors.content")
  :components
  ((:file "vivid-colors.content"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :vivid-colors.content args)))
