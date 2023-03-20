(in-package #:hoop)

(defclass generator-clause (var-spec-slot using-form-slot temp-var-slot)
  ((then :reader then
         :initarg :then)))

(defmethod make-clause ((type (eql :generate)) &rest initargs)
  (apply #'make-instance 'generator-clause :var-spec initargs))

(defmethod wrap-form ((clause generator-clause) form)
  (if (listp (var-spec clause))
      `(let ((,(temp-var clause) (multiple-value-list ,(using-form clause))))
         (symbol-macrolet ,(symbol-macros-from-d-var-spec (var-spec clause)
                                                          (temp-var clause))
           ,form))
      `(let ((,(var-spec clause) ,(using-form clause)))
         ,form)))

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
