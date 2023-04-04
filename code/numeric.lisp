(in-package #:hoop)

(defclass numeric-clause (var-spec-slot)
  ())

(defmethod return-value-forms ((clause numeric-clause))
  (list (var-spec clause)))

(defclass count-clause (numeric-clause)
  ())

(defmethod make-clause ((type (eql :count)) &rest initargs)
  (apply #'make-instance 'count-clause :var-spec initargs))

(defmethod wrap-outer-form ((clause count-clause) form)
  `(let ((,(var-spec clause) (coerce 0 ',(get-type (var-spec clause)))))
     ,.(apply #'declarations
              (bindings-from-d-var-spec (var-spec clause)))
     (flet ((,(var-spec clause) (&rest args)
              (incf ,(var-spec clause) (count-if #'identity args))))
       ,form)))

(defmethod declaration-targets ((clause count-clause))
  (bindings-from-d-var-spec (var-spec clause)))

(defclass narg-numeric-clause (numeric-clause from-form-slot)
  ((operator :reader operator
             :initarg :operator)))

(defmethod wrap-inner-form ((clause narg-numeric-clause) form)
  `(let ((,(var-spec clause) (coerce ,(from-form clause)
                                     ',(get-type (var-spec clause)))))
     ,.(apply #'declarations
              (bindings-from-d-var-spec (var-spec clause)))
     (flet ((,(var-spec clause) (&rest args)
              (setf ,(var-spec clause)
                    (if ,(var-spec clause)
                        (apply ,(operator clause) ,(var-spec clause) args)
                        (apply ,(operator clause) args)))))
       ,form)))

(defmethod declaration-targets ((clause narg-numeric-clause))
  (bindings-from-d-var-spec (var-spec clause)))

(defmethod make-clause ((type (eql :sum)) &rest initargs)
  (apply #'make-instance 'narg-numeric-clause
         :operator '(function +) :from 0 :var-spec initargs))

(defmethod make-clause ((type (eql :product)) &rest initargs)
  (apply #'make-instance 'narg-numeric-clause
         :operator '(function *) :from 1 :var-spec initargs))

(defmethod make-clause ((type (eql :maximize)) &rest initargs)
  (apply #'make-instance 'narg-numeric-clause
         :operator '(function max) :from nil :var-spec initargs))

(defmethod make-clause ((type (eql :minimize)) &rest initargs)
  (apply #'make-instance 'narg-numeric-clause
         :operator '(function min) :from nil :var-spec initargs))

