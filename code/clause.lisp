(in-package #:hoop)

(defgeneric make-clause (type &rest initargs))

(defgeneric wrap-form (clause form)
  (:method (clause form)
    (declare (ignore clause))
    form))

(defgeneric termination-forms (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric prologue-forms (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric before-forms (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric after-forms (clause)
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
    (values nil nil)))

(defgeneric block-name (clause)
  (:method (clause)
    (declare (ignore clause))
    (values nil nil)))

(defclass var-spec-slot ()
  ((var-spec :accessor var-spec
             :initarg :var-spec
             :type (or symbol cons))))

(defclass equals-form-slot ()
  ((equals :accessor equals-form
           :initarg :=)))

(defclass using-form-slot ()
  ((using :accessor using-form
          :initarg :using)))

(defclass by-slots ()
  ((by-var :accessor by-var
           :initform (gensym))
   (by-form :accessor by-form
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

(defclass temp-var-slot ()
  ((temp-var :reader temp-var
             :initform (gensym))))

(defclass clause ()
  ((initargs-order :accessor initargs-order)))

(defmethod initialize-instance :after ((instance clause) &rest initargs &key)
  (setf (initargs-order instance)
        (loop for key in initargs by #'cddr
              collect key)))              

(defun assemble-in-order (clause mapping)
  (mapcan (lambda (indicator)
            (getf mapping indicator))
          (initargs-order clause)))

(defun bindings-from-d-var-spec (var-spec &optional form)
  (check-type var-spec (or symbol cons))
  (cond ((null var-spec)
         nil)
        ((symbolp var-spec)
         `((,var-spec ,form)))
        (t
         (nconc (bindings-from-d-var-spec (car var-spec) `(car ,form))
                (bindings-from-d-var-spec (cdr var-spec) `(cdr ,form))))))

(defgeneric variable-names (object)
  (:method (object)
    (declare (ignore object))
    nil)
  (:method ((object symbol))
    (when object
      (list object)))
  (:method ((object cons))
    (nconc (variable-names (car object))
           (variable-names (cdr object))))
  (:method ((object var-spec-slot))
    (variable-names (var-spec object))))

