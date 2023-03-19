(in-package #:hoop)

(defclass equals-clause (clause)
  ((temp-var :reader temp-var
             :initform (gensym))))

(defmethod expand (var (action (eql :=)) &optional initform &rest initargs &key &allow-other-keys)
  (apply #'make-instance 'equals-clause
         :var var :initform initform initargs))

(defmethod bindings ((clause equals-clause))
  (if (listp (var clause))
      `((,(temp-var clause) (multiple-value-list ,(initform clause))))
      `((,(var clause) ,(initform clause)))))

(defmethod wrap-inner ((clause equals-clause) form)
  (if (listp (var clause))
      `(symbol-macrolet ,(symbol-macros-from-d-var-spec (var clause) (temp-var clause))
         ,form)
      form))

