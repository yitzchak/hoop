(in-package #:hoop)

(defclass numeric-clause (var-spec-slot)
  ())

(defmethod return-form ((clause numeric-clause))
  (values t (var-spec clause)))

(defclass count-clause (numeric-clause)
  ())

(defmethod make-clause (parallel (type (eql :count)) &rest initargs)
  (apply #'make-instance 'count-clause :var-spec initargs))

(defmethod wrap-outer-form ((clause count-clause) form)
  `(let ((,(var-spec clause) 0))
     (flet ((,(var-spec clause) (&rest args)
              (incf ,(var-spec clause) (count-if #'identity args))))
       ,form)))

(defclass narg-numeric-clause (numeric-clause from-form-slot)
  ((operator :reader operator
             :initarg :operator)))

(defmethod wrap-inner-form ((clause narg-numeric-clause) form)
  `(let ((,(var-spec clause) ,(from-form clause)))
     (flet ((,(var-spec clause) (&rest args)
              (setf ,(var-spec clause)
                    (if ,(var-spec clause)
                        (apply ,(operator clause) ,(var-spec clause) args)
                        (apply ,(operator clause) args)))))
       ,form)))

(defmethod make-clause (parallel (type (eql :sum)) &rest initargs)
  (apply #'make-instance 'narg-numeric-clause
         :operator '(function +) :from 0 :var-spec initargs))

(defmethod make-clause (parallel (type (eql :product)) &rest initargs)
  (apply #'make-instance 'narg-numeric-clause
         :operator '(function *) :from 1 :var-spec initargs))

(defmethod make-clause (parallel (type (eql :maximize)) &rest initargs)
  (apply #'make-instance 'narg-numeric-clause
         :operator '(function max) :from nil :var-spec initargs))

(defmethod make-clause (parallel (type (eql :minimize)) &rest initargs)
  (apply #'make-instance 'narg-numeric-clause
         :operator '(function min) :from nil :var-spec initargs))

