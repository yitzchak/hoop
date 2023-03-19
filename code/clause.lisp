(in-package #:hoop)

(defgeneric make-clause (keyword &rest initargs))

(defgeneric wrap-inner (clause form)
  (:method (clause form)
    (declare (ignore clause))
    form))

(defgeneric wrap-outer (clause form)
  (:method (clause form)
    (declare (ignore clause))
    form))

(defgeneric bindings (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric declarations (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric prologue-forms (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric epilogue-forms (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric return-form (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defclass var-spec-slot ()
  ((var-spec :accessor var-spec
             :initarg :var-spec
             :type (or symbol cons))))

(defclass initform-slot ()
  ((initform :accessor initform
             :initarg :=)))

(defclass by-form-slot ()
  ((by-form :accessor by-form
            :initarg :by)))

(defclass into-form-slot ()
  ((into-form :accessor into-form
              :initarg :into)))

(defclass in-form-slot ()
  ((in-form :accessor in-form
            :initarg :in)))

(defclass on-form-slot ()
  ((on-form :accessor on-form
            :initarg :on)))

(defclass from-form-slot ()
  ((from-form :accessor from-form
            :initarg :from)))

(defclass clause ()
  ((var :reader var
        :initarg :var)
   (initform :reader initform
             :initarg :initform)))

(defun symbol-macros-from-d-var-spec (var-spec form)
  (check-type var-spec (or symbol cons))
  (cond ((null var-spec)
         nil)
        ((symbolp var-spec)
         `((,var-spec ,form)))
        (t
         (nconc (symbol-macros-from-d-var-spec (car var-spec) `(car ,form))
                (symbol-macros-from-d-var-spec (cdr var-spec) `(cdr ,form))))))
