(in-package #:hoop)

(defclass with-clause (var-spec-slot equals-form-slot temp-var-slot)
  ())

(defmethod make-clause ((keyword (eql :with)) &rest initargs)
  (apply #'make-instance 'with-clause :var-spec initargs))

(defmethod wrap-form ((clause with-clause) form)
  (if (listp (var-spec clause))
      `(let ((,(temp-var clause) (multiple-value-list ,(equals-form clause))))
         (symbol-macrolet ,(symbol-macros-from-d-var-spec (var-spec clause) (temp-var clause))
           ,form))
      `(let ((,(var-spec clause) ,(equals-form clause)))
         ,form)))

