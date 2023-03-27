(in-package #:hoop)

(defclass with-clause (var-spec-slot equals-form-slot temp-var-slot)
  ()
  (:default-initargs := nil))

(defclass parallel-with-clause (with-clause)
  ())

(defmethod make-clause (parallel (keyword (eql :with)) &rest initargs)
  (apply #'make-instance (if parallel
                             'parallel-with-clause
                             'with-clause)
         :var-spec initargs))

(defmethod wrap-outer-form ((clause with-clause) form)
  (if (listp (var-spec clause))
      `(let ((,(temp-var clause) (multiple-value-list ,(equals-form clause)))
             ,.(bindings-from-d-var-spec (var-spec clause)
                                         (temp-var clause)))
         ,form)
      `(let ((,(var-spec clause) ,(equals-form clause)))
         ,form)))

(defmethod wrap-outer-form ((clause parallel-with-clause) form)
  (if (listp (var-spec clause))
      `(let ((,(temp-var clause) (multiple-value-list ,(equals-form clause))))
         ,form)
      `(let ((,(temp-var clause) ,(equals-form clause)))
         ,form)))

(defmethod wrap-inner-form ((clause parallel-with-clause) form)
  `(let* ,(bindings-from-d-var-spec (var-spec clause)
                                    (temp-var clause))
     ,form))
