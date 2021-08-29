(in-package :cl-user)

(defpackage :vivid-colors
  (:use :cl)
  (:export))

(in-package :vivid-colors)

(defparameter *vprint-dispatch* nil)

;;;; VPRINTER

(defstruct vprinter
  (type (error "TYPE is required.") :read-only t)
  (function (error "FUNCTION is required.")
            :type (or function symbol)
            :read-only t)
  (priority 0 :type real :read-only t))

(defun set-vprint-dispatch (type function &optional (priority 0))
  (push (make-vprinter :type type :function function :priority priority)
        *vprint-dispatch*)
  t)
