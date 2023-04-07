(in-package #:hoop)

(defclass generate-clause (var-spec-slot using-form-slot temp-var-slot)
  ((then-form :accessor then-form
              :initarg :then)))

(defmethod initialize-instance :after ((instance generate-clause) &rest initargs &key)
  (declare (ignore initargs))
  (unless (slot-boundp instance 'then-form)
    (setf (then-form instance) (using-form instance))))

(defmethod make-clause ((type (eql :generate)) &rest initargs)
  (apply #'make-instance 'generate-clause :var-spec initargs))

(defmethod declaration-targets ((clause generate-clause))
  (variable-names (var-spec clause)))

(defmethod wrap-outer-form ((clause generate-clause) form)
  (if (listp (var-spec clause))
      `(let ((,(temp-var clause) (multiple-value-list ,(using-form clause))))
         ,form)
      `(let ((,(temp-var clause) ,(using-form clause)))
         ,form)))

(defmethod wrap-inner-form ((clause generate-clause) form)
  `(let ,(bindings-from-d-var-spec (var-spec clause) (temp-var clause))
     ,.(declarations (variable-names (var-spec clause)))
     ,form))

(defmethod next-movable-forms ((clause generate-clause))
  (if (listp (var-spec clause))
      `((setq ,(temp-var clause) (multiple-value-list ,(then-form clause))
              ,.(apply #'nconc (bindings-from-d-var-spec (var-spec clause)
                                                         (temp-var clause)))))
      `((setq ,(var-spec clause) ,(then-form clause)))))
