(in-package #:hoop)

(defclass with-clause ()
  ((subclauses :accessor subclauses
               :initarg :subclauses)))

(defclass with-subclause (var-spec-slot equals-form-slot temp-var-slot)
  ()
  (:default-initargs := nil))

(defmethod variable-names ((clause with-clause))
  (mapcan #'variable-names (subclauses clause)))

(defmethod make-clause ((keyword (eql :with)) &rest initargs)
  (prog (subclauses)
   next
     (cond ((eq (second initargs) :=)
            (push (make-instance 'with-subclause
                                 :var-spec (first initargs)
                                 := (third initargs))
                  subclauses)
            (setf initargs (cdddr initargs)))
           (t
            (push (make-instance 'with-subclause
                                 :var-spec (pop initargs))
                  subclauses)))
     (unless initargs
       (return (make-instance 'with-clause :subclauses (nreverse subclauses))))
     (assert (eq (pop initargs) :and))
     (go next)))

(defmethod wrap-form ((clause with-clause) form)
  (let ((initial-bindings (mapcar (lambda (subclause)
                                    (if (listp (var-spec subclause))
                                        `(,(temp-var subclause) (multiple-value-list ,(equals-form subclause)))
                                        `(,(var-spec subclause) ,(equals-form subclause))))
                                  (subclauses clause)))
        (secondary-bindings (mapcan (lambda (subclause)
                                      (when (listp (var-spec subclause))
                                        (bindings-from-d-var-spec (var-spec subclause)
                                                                  (temp-var subclause))))
                                    (subclauses clause))))
    (if secondary-bindings
        `(let ,initial-bindings
           (let ,secondary-bindings
             ,form))
        `(let ,initial-bindings
           ,form))))
