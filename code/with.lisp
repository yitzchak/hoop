(in-package #:hoop)

(defclass with-clause (var-spec-slot initform-slot)
  ((temp-var :reader temp-var
             :initform (gensym))))

(defmethod make-clause ((keyword (eql :with)) &rest initargs)
  (apply #'make-instance 'with-clause :var-spec initargs))

(defmethod bindings ((clause with-clause))
  (if (listp (var-spec clause))
      `((,(temp-var clause) (multiple-value-list ,(initform clause))))
      `((,(var-spec clause) ,(initform clause)))))

(defmethod wrap-inner ((clause with-clause) form)
  (if (listp (var-spec clause))
      `(symbol-macrolet ,(symbol-macros-from-d-var-spec (var-spec clause) (temp-var clause))
         ,form)
      form))

