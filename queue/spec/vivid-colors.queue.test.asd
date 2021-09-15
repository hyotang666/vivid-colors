; vim: ft=lisp et
(in-package :asdf)
(defsystem "vivid-colors.queue.test"
  :version
  "0.0.1"
  :depends-on
  (:jingoh "vivid-colors.queue")
  :components
  ((:file "vivid-colors.queue"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :vivid-colors.queue args)))
