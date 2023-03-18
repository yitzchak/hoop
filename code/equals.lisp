(in-package #:hoop)

(defclass equals-clause (clause)
  ())

(defmethod expand (var (action (eql :=)) &optional initform &rest initargs &key &allow-other-keys)
  (apply #'make-instance 'equals-clause
         :var var :initform initform initargs))

(defmethod bindings ((clause equals-clause))  
  `((,(var clause) ,(initform clause))))
