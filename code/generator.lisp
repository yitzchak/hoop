(in-package #:hoop)

(defclass generator-clause (var-spec-slot initform-slot)
  ((then :reader then
         :initarg :then)
   (%while :reader %while
           :initarg :while)
   (temp-var :reader temp-var
             :initform (gensym))))

(defmethod make-clause ((keyword (eql :generator)) &rest initargs)
  (apply #'make-instance 'generator-clause :var-spec initargs))

(defmethod bindings ((clause generator-clause))
  (if (listp (var-spec clause))
      `((,(temp-var clause) (multiple-value-list ,(initform clause))))
      `((,(var-spec clause) ,(initform clause)))))

(defmethod wrap-inner ((clause generator-clause) form)
  (if (listp (var-spec clause))
      `(symbol-macrolet ,(symbol-macros-from-d-var-spec (var-spec clause) (temp-var clause))
         ,form)
      form))

(defmethod prologue-forms ((clause generator-clause))
  (when (slot-boundp clause '%while)
    `((unless ,(%while clause)
        (hoop-finish)))))

(defmethod epilogue-forms ((clause generator-clause))
  (if (listp (var-spec clause))
      `((setq ,(temp-var clause)
              (multiple-value-list ,(if (slot-boundp clause 'then)
                                        (then clause)
                                        (initform clause)))))      
      `((setq ,(var-spec clause)
              ,(if (slot-boundp clause 'then)
                   (then clause)
                   (initform clause))))))
