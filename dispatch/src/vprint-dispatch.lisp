(in-package :cl-user)

(defpackage :vivid-colors.dispatch
  (:use :cl)
  (:export #:dispatch-key-confliction ; condition.
           #:replace-by-new ; restart-function
           #:keep-old ; restart-function
           #:vprint-dispatch ; structure name / function
           #:find-vprint-dispatch
           #:*default-printer*
           #:*vprint-dispatch*
           #:define-vprint-dispatch ; dsl
           #:in-vprint-dispatch))

(in-package :vivid-colors.dispatch)

(declaim (optimize speed))

;;;; CONDITION

(define-condition dispatch-key-confliction (error)
  ((type :initarg :type :reader conflicted-type))
  (:report
   (lambda (this output)
     (funcall (formatter "VPRINT-DISPATCH key is conflicted. ~S") output
              (conflicted-type this)))))

;; RESTART functions

(defun replace-by-new (condition)
  (let ((restart (find-restart 'replace condition)))
    (when restart
      (invoke-restart restart))))

(defun keep-old (condition)
  (let ((restart (find-restart 'ignore condition)))
    (when restart
      (invoke-restart restart))))

;;;; VPRINTER object.

(locally ; Out of responds.
 (declare (optimize (speed 1)))
 (defstruct vprinter
   (type (error "TYPE is required.") :read-only t)
   (function (error "FUNCTION is required.")
             :type (or function symbol)
             :read-only t)
   (priority 0 :type real :read-only t)))

;;;; VPRINT-DISPATCH object.

(defstruct (vprint-dispatch (:copier nil))
  (name (error "NAME is required.") :type symbol)
  (table (make-hash-table :test #'equal) :type hash-table :read-only t))

(defmethod print-object ((this vprint-dispatch) output)
  (cond (*print-readably* (call-next-method))
        (t
         (print-unreadable-object (this output :type t :identity nil)
           (funcall (formatter "~W entry ~D") output
                    (vprint-dispatch-name this)
                    (hash-table-count (vprint-dispatch-table this)))))))

(defmacro dotable ((var <vprint-dispatch> &optional <return>) &body body)
  `(hash-table-ext:doht (,var (vprint-dispatch-table ,<vprint-dispatch>)
                         ,<return>)
     ,@body))

;;;; Underlying database.

(defvar *vprint-dispatch-repository* (make-hash-table :test #'eq))

;;;; CRUD for database.
;; Refer.

(declaim
 (ftype (function (symbol) (values (or null vprint-dispatch) &optional))
        find-vprint-dispatch))

(defun find-vprint-dispatch (name)
  #+clisp
  (check-type name symbol)
  (values (gethash name *vprint-dispatch-repository*)))

(defun list-all-vprint-dispatches ()
  (alexandria:hash-table-keys *vprint-dispatch-repository*))

;; Update.

(declaim
 (ftype (function (symbol vprint-dispatch) (values vprint-dispatch &optional))
        store-vprint-dispatch))

(defun store-vprint-dispatch (name vprint-dispatch)
  #+clisp
  (progn (check-type name symbol) (check-type vprint-dispatch vprint-dispatch))
  (setf (gethash name *vprint-dispatch-repository*) vprint-dispatch))

;; Miscellaneous.

(declaim
 (ftype (function (vprint-dispatch &rest vprint-dispatch)
         (values vprint-dispatch &optional))
        merge-vprint-dispatch))

(defun merge-vprint-dispatch (vprint-dispatch &rest rest)
  (let ((vprint-dispatch (copy-vprint-dispatch vprint-dispatch)))
    (dolist (dispatch rest vprint-dispatch)
      (dotable ((key-type vprinter) dispatch)
        (if (gethash key-type (vprint-dispatch-table vprint-dispatch))
            (restart-case (error 'dispatch-key-confliction :type key-type)
              (replace ()
                  :report "Replace by new one."
                (setf (gethash key-type
                               (vprint-dispatch-table vprint-dispatch))
                        vprinter))
              (ignore () :report "Ignore new one."))
            (setf (gethash key-type (vprint-dispatch-table vprint-dispatch))
                    vprinter))))))

;;;; Current vprint-dispatch.

(declaim (type vprint-dispatch *vprint-dispatch*))

(defvar *vprint-dispatch*)

;; Modify

(declaim
 (ftype (function
         ((or cons symbol) (or symbol function) &optional real vprint-dispatch)
         (values null &optional))
        set-vprint-dispatch))

(defun set-vprint-dispatch
       (type function &optional (priority 0) (table *vprint-dispatch*))
  #+clisp
  (progn (check-type function (or symbol function)) (check-type priority real))
  (assert (millet:type-specifier-p type))
  (remhash type (vprint-dispatch-table table))
  (when function
    (setf (gethash type (vprint-dispatch-table table))
            (make-vprinter :type type :function function :priority priority)))
  nil)

;; DSL

(defmacro define-vprint-dispatch (name &body clause+)
  (declare (type list clause+))
  ;; Trivial syntax check.
  (check-type name symbol)
  (assert (<= (count :merge clause+ :key #'car) 1))
  (assert (every (lambda (clause) (find (car clause) '(:merge :set))) clause+))
  `(let ((*vprint-dispatch*
          ,(let ((merge (find :merge clause+ :key #'car)))
             (if merge
                 `(merge-vprint-dispatch (make-vprint-dispatch :name ',name)
                                         ,@(mapcar
                                             (lambda (x)
                                               `(find-vprint-dispatch ',x))
                                             (cdr merge)))
                 `(make-vprint-dispatch :name ',name)))))
     ,@(loop :for clause :in clause+
             :if (eq :set (car clause))
               :collect `(set-vprint-dispatch ,@(cdr clause)))
     (store-vprint-dispatch ',name *vprint-dispatch*)
     ',name))

(defmacro in-vprint-dispatch (name)
  `(setq *vprint-dispatch*
           (or (find-vprint-dispatch ',name)
               (error "Missing VPRINT-DISPATCH named ~S." ',name))))

;;;; VPRINT-DISPATCH.

(defvar *default-printer*)

(declaim
 (ftype (function (t &optional vprint-dispatch) (values list &optional))
        vprinters))

(defun vprinters (exp &optional (vprint-dispatch *vprint-dispatch*))
  ;; For debug use.
  (let* ((head (cons :head nil)) (tail head))
    (dotable ((key-type vprinter) vprint-dispatch (cdr head))
      (declare (optimize (speed 1))) ; key-type is run time value.
      (when (typep exp key-type)
        (rplacd tail (setf tail (list vprinter)))))))

(declaim
 (ftype (function (t &optional vprint-dispatch)
         (values (or symbol function) boolean &optional))
        vprint-dispatch))

(defun vprint-dispatch (exp &optional (vprint-dispatch *vprint-dispatch*))
  (let ((vprinters
         (sort (vprinters exp vprint-dispatch) #'subtypep
               :key #'vprinter-type)))
    (cond ((null vprinters) (values *default-printer* nil))
          ((null (cdr vprinters)) ; one element.
           (values (vprinter-function (car vprinters)) t))
          ((or (and (subtypep (vprinter-type (first vprinters))
                              (vprinter-type (second vprinters)))
                    (not
                      (subtypep (vprinter-type (second vprinters))
                                (vprinter-type (first vprinters)))))
               (not
                 (subtypep (vprinter-type (first vprinters))
                           (vprinter-type (second vprinters)))))
           (values (vprinter-function (car vprinters)) t))
          ((locally ; due to real.
            (declare (optimize (speed 1)))
            (< (vprinter-priority (first vprinters))
               (vprinter-priority (second vprinters))))
           (values (vprinter-function (car vprinters)) t))
          (t
           (warn "Could not determine vprinters. ~S" vprinters)
           (values (vprinter-function (car vprinters)) t)))))

(declaim
 (ftype (function (&optional (or null vprint-dispatch))
         (values vprint-dispatch &optional))
        copy-vprint-dispatch))

(defun copy-vprint-dispatch (&optional (vprint-dispatch *vprint-dispatch*))
  (make-vprint-dispatch :name (if vprint-dispatch
                                  (vprint-dispatch-name vprint-dispatch)
                                  :standard)
                        :table (alexandria:copy-hash-table
                                 (if vprint-dispatch
                                     (vprint-dispatch-table vprint-dispatch)
                                     (vprint-dispatch-table
                                       (find-vprint-dispatch :standard))))))
