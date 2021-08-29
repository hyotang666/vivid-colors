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

(defun vprint-dispatch (exp &optional (dispatch-table *vprint-dispatch*))
  (loop :for vprinter :in dispatch-table
        :if (typep exp (vprinter-type vprinter))
          :collect vprinter :into vprinters
        :finally (setf vprinters
                         (sort vprinters #'subtypep :key #'vprinter-type))
                 (return
                  (cond ((null vprinters) 'default-printer)
                        ((null (cdr vprinters)) ; one element.
                         (vprinter-function (car vprinters)))
                        ((or (and (subtypep (vprinter-type (first vprinters))
                                            (vprinter-type (second vprinters)))
                                  (not
                                    (subtypep
                                      (vprinter-type (second vprinters))
                                      (vprinter-type (first vprinters)))))
                             (not
                               (subtypep (vprinter-type (first vprinters))
                                         (vprinter-type (second vprinters)))))
                         (vprinter-function (car vprinters)))
                        ((< (vprinter-priority (first vprinters))
                            (vprinter-priority (second vprinters)))
                         (vprinter-function (car vprinters)))
                        (t
                         (error "Could not determine vprinters. ~S"
                                vprinters))))))
