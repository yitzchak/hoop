(in-package #:hoop)

(defclass named-clause ()
  ((named :reader named
          :initarg :named)))

(defmethod make-clause (parallel (type (eql :named)) &rest initargs)
  (apply #'make-instance 'named-clause :named initargs))

(defmethod block-name ((clause named-clause))
  (values t (named clause)))
