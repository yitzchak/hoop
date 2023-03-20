(in-package #:hoop)

(defclass package-clause (var-spec-slot in-form-slot)
  ((symbol-types :reader symbol-types
                 :initform '(:external)
                 :initarg :symbol-types)
   (iterator-var :reader iterator-var
                 :initform (gensym))
   (successp-var :reader successp-var
                 :initform (gensym))
   (status-var :reader status-var
               :initarg :status
               :initform (gensym))
   (package-var :reader package-var
                :initarg :package
                :initform (gensym))))

(defmethod make-clause ((type (eql :each-symbol)) &rest initargs)
  (apply #'make-instance 'package-clause :var-spec initargs))

(defmethod wrap-outer ((clause package-clause) form)
  `(with-package-iterator (,(iterator-var clause) ,(in-form clause) ,@(symbol-types clause))
     ,form))

(defmethod bindings ((clause package-clause))
  `(,(var-spec clause) ,(successp-var clause) ,(status-var clause) ,(package-var clause)))

(defmethod prologue-forms ((clause package-clause))
  `((multiple-value-setq (,(successp-var clause) ,(var-spec clause)
                          ,(status-var clause) ,(package-var clause))
      (,(iterator-var clause)))
    (unless ,(successp-var clause)
      (hoop-finish))))
