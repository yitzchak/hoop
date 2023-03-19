(in-package #:hoop)

(defclass execution-clause ()
  ((forms :reader forms
          :initarg :forms)))

(defclass prologue-clause (execution-clause)
  ())

(defmethod make-clause ((type (eql :prologue)) &rest initargs)
  (make-instance 'prologue-clause :forms initargs))

(defmethod prologue-forms ((clause prologue-clause))
  (forms clause))

(defclass epilogue-clause (execution-clause)
  ())

(defmethod make-clause ((type (eql :epilogue)) &rest initargs)
  (make-instance 'epilogue-clause :forms initargs))

(defmethod epilogue-forms ((clause epilogue-clause))
  (forms clause))

(defclass finish-clause (execution-clause)
  ())

(defmethod make-clause ((type (eql :finish)) &rest initargs)
  (make-instance 'finish-clause :forms initargs))

(defmethod finish-forms ((clause finish-clause))
  (forms clause))

