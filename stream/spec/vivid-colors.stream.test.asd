; vim: ft=lisp et
(in-package :asdf)
(defsystem "vivid-colors.stream.test"
  :version
  "0.1.2"
  :depends-on
  (:jingoh "vivid-colors.stream")
  :components
  ((:file "vivid-colors.stream"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :vivid-colors.stream args)))
