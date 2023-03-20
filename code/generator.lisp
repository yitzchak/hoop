(in-package #:hoop)

(defclass generator-clause (var-spec-slot using-form-slot temp-var-slot)
  ((then :reader then
         :initarg :then)))

(defmethod make-clause ((type (eql :generate)) &rest initargs)
  (apply #'make-instance 'generator-clause :var-spec initargs))

(defmethod bindings ((clause generator-clause))
  (if (listp (var-spec clause))
      `((,(temp-var clause) (multiple-value-list ,(using-form clause))))
      `((,(var-spec clause) ,(using-form clause)))))

(defmethod wrap-form ((clause generator-clause) form)
  (if (listp (var-spec clause))
      `(symbol-macrolet ,(symbol-macros-from-d-var-spec (var-spec clause) (temp-var clause))
         ,form)
      form))

(defmethod epilogue-forms ((clause generator-clause))
  (if (listp (var-spec clause))
      `((setq ,(temp-var clause)
              (multiple-value-list ,(if (slot-boundp clause 'then)
                                        (then clause)
                                        (using-form clause)))))      
      `((setq ,(var-spec clause)
              ,(if (slot-boundp clause 'then)
                   (then clause)
                   (equals-form clause))))))
