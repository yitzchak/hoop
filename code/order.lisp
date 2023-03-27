(in-package #:hoop)

(defclass order-clause ()
  ((subclauses :reader subclauses
               :initarg :subclauses)))

(defclass parallel-clause (order-clause)
  ())

(defclass serial-clause (order-clause)
  ())

(defmethod make-clause (parallel (type (eql :parallel)) &rest initargs)
  (make-instance 'parallel-clause
                 :subclauses (mapcar (lambda (args)
                                       (apply #'make-clause t args))
                                     initargs)))

(defmethod make-clause (parallel (type (eql :serial)) &rest initargs)
  (make-instance 'serial-clause
                 :subclauses (mapcar (lambda (args)
                                       (apply #'make-clause nil args))
                                     initargs)))

(defmethod wrap-outer-form ((clause parallel-clause) form)
  (reduce #'wrap-outer-form (subclauses clause)
          :from-end t
          :initial-value form))

(defmethod wrap-inner-form ((clause parallel-clause) form)
  (reduce #'wrap-inner-form (subclauses clause)
          :from-end t :initial-value form))

(defmethod wrap-outer-form ((clause serial-clause) form)
  (reduce (lambda (subclause form)
            (wrap-outer-form subclause
                             (wrap-inner-form subclause
                                              form)))
          (subclauses clause)
          :from-end t
          :initial-value form))

(defmethod initial-early-forms ((clause parallel-clause))
  (mapcan (lambda (subclause)
            (nconc (initial-early-forms subclause)
                   (initial-movable-forms subclause)))
          (subclauses clause)))

(defmethod initial-late-forms ((clause parallel-clause))
  (mapcan #'initial-late-forms (subclauses clause)))

(defmethod next-early-forms ((clause parallel-clause))
  (mapcan (lambda (subclause)
            (nconc (next-early-forms subclause)
                   (next-movable-forms subclause)))
          (subclauses clause)))

(defmethod next-late-forms ((clause parallel-clause))
  (mapcan #'next-late-forms (subclauses clause)))

(defmethod initial-early-forms ((clause serial-clause))
  (mapcan #'initial-early-forms (subclauses clause)))

(defmethod initial-late-forms ((clause serial-clause))
  (mapcan (lambda (subclause)
            (nconc (initial-movable-forms subclause)
                   (initial-late-forms subclause)))
          (subclauses clause)))

(defmethod next-early-forms ((clause serial-clause))
  (mapcan #'next-early-forms (subclauses clause)))

(defmethod next-late-forms ((clause serial-clause))
  (mapcan (lambda (subclause)
            (nconc (next-movable-forms subclause)
                   (next-late-forms subclause)))
          (subclauses clause)))

(defmethod prologue-forms ((clause order-clause))
  (mapcan #'prologue-forms (subclauses clause)))

(defmethod before-forms ((clause order-clause))
  (mapcan #'before-forms (subclauses clause)))

(defmethod after-forms ((clause order-clause))
  (mapcan #'after-forms (subclauses clause)))

(defmethod epilogue-forms ((clause order-clause))
  (mapcan #'epilogue-forms (subclauses clause)))

(defmethod return-form ((clause order-clause))
  (dolist (subclause (subclauses clause))
    (multiple-value-bind (returnp form)
        (return-form subclause)
      (when returnp
        (return-from return-form (values t form)))))
  (values nil nil))

(defmethod block-name ((clause order-clause))
  (dolist (subclause (subclauses clause))
    (multiple-value-bind (validp name)
        (block-name subclause)
      (when validp
        (return-from block-name (values t name)))))
  (values nil nil))

(defmethod variable-names ((clause order-clause))
  (mapcan #'variable-names (subclauses clause)))
