(in-package #:hoop)

(defclass list-clause (var-spec-slot by-slots)
  ((list-var :reader list-var
             :initform (gensym)))
  (:default-initargs :by #'cdr))

(defclass in-clause (list-clause in-form-slot)
  ())

(defclass on-clause (list-clause on-form-slot)
  ())

(defmethod make-clause ((keyword (eql :each-item)) &rest initargs)
  (apply #'make-instance (if (get-properties (cdr initargs) '(:in))
                             'in-clause
                             'on-clause)
         :var-spec initargs))

(defmethod bindings ((clause in-clause))
  `((,(list-var clause) ,(in-form clause))
    (,(by-var clause) ,(by-form clause))))

(defmethod bindings ((clause on-clause))
  `((,(list-var clause) ,(on-form clause))
    (,(by-var clause) ,(by-form clause))))

(defmethod wrap-inner ((clause in-clause) form)
  `(symbol-macrolet ,(symbol-macros-from-d-var-spec (var-spec clause)
                                                    `(car ,(list-var clause)))
     ,form))

(defmethod wrap-inner ((clause on-clause) form)
  `(symbol-macrolet ,(symbol-macros-from-d-var-spec (var-spec clause)
                                                    (list-var clause))
     ,form))

(defmethod prologue-forms ((clause list-clause))
  `((unless ,(list-var clause) (hoop-finish))))

(defmethod epilogue-forms ((clause list-clause))
  `((setf ,(list-var clause) (funcall ,(by-var clause) ,(list-var clause)))))

