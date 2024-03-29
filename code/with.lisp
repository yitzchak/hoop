(in-package #:hoop)

(defclass with-clause (var-spec-slot equals-form-slot temp-var-slot)
  ()
  (:default-initargs := nil))


(defmethod make-clause ((keyword (eql :with)) &rest initargs)
  (apply #'make-instance 'with-clause
         :var-spec initargs))

(defmethod wrap-outer-form ((clause with-clause) form)
  (if (listp (var-spec clause))
      `(let ((,(temp-var clause) (multiple-value-list ,(equals-form clause))))
         ,form)
      `(let ((,(temp-var clause) ,(equals-form clause)))
         ,form)))

(defmethod wrap-inner-form ((clause with-clause) form)
  `(let ,(bindings-from-d-var-spec (var-spec clause)
                                   (temp-var clause))
     ,.(declarations (variable-names (var-spec clause)))
     ,form))

(defmethod declaration-targets ((clause with-clause))
  (variable-names (var-spec clause)))
