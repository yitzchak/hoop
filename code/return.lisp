(in-package #:hoop)

(defclass return-clause ()
  ((return-form :reader return-form
                :initarg :return
                :initform nil)))

(defmethod make-clause ((keyword (eql :return)) &rest initargs)
  (apply #'make-instance 'return-clause :return initargs))

