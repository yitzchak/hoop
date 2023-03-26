(in-package #:hoop)

(defclass execution-clause ()
  ((forms :reader forms
          :initarg :forms)))

(defclass prologue-clause (execution-clause)
  ())

(defmethod make-clause (parallel (type (eql :prologue)) &rest initargs)
  (make-instance 'prologue-clause :forms initargs))

(defmethod prologue-forms ((clause prologue-clause))
  (forms clause))

(defclass before-clause (execution-clause)
  ())

(defmethod make-clause (parallel (type (eql :before)) &rest initargs)
  (make-instance 'before-clause :forms initargs))

(defmethod before-forms ((clause before-clause))
  (forms clause))

(defclass after-clause (execution-clause)
  ())

(defmethod make-clause (parallel (type (eql :after)) &rest initargs)
  (make-instance 'after-clause :forms initargs))

(defmethod after-forms ((clause after-clause))
  (forms clause))

(defclass epilogue-clause (execution-clause)
  ())

(defmethod make-clause (parallel (type (eql :epilogue)) &rest initargs)
  (make-instance 'epilogue-clause :forms initargs))

(defmethod epilogue-forms ((clause epilogue-clause))
  (forms clause))

