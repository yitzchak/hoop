(in-package #:hoop)

(defclass numeric-clause (var-spec-slot)
  ())

(defmethod return-form ((clause numeric-clause))
  (values t (var-spec clause)))

(defclass count-clause (numeric-clause)
  ())

(defmethod make-clause ((type (eql :count)) &rest initargs)
  (apply #'make-instance 'count-clause :var-spec initargs))

(defmethod bindings ((clause count-clause))  
  `((,(var-spec clause) 0)))

(defmethod wrap-inner ((clause count-clause) form)
  `(flet ((,(var-spec clause) (&rest args)
            (incf ,(var-spec clause) (length args))))
     ,form))

(defclass narg-numeric-clause (numeric-clause from-form-slot)
  ((operator :reader operator
             :initarg :operator)))

(defmethod bindings ((clause narg-numeric-clause))  
  `((,(var-spec clause) ,(from-form clause))))

(defmethod wrap-inner ((clause narg-numeric-clause) form)
  `(flet ((,(var-spec clause) (&rest args)
            (setf ,(var-spec clause)
                  (if ,(var-spec clause)
                      (apply ,(operator clause) ,(var-spec clause) args)
                      (apply ,(operator clause) args)))))
     ,form))

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

