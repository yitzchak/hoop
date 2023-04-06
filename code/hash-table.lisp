(in-package #:hoop)

(defclass hash-table-clause (var-spec-slot in-form-slot temp-var-slot)
  ((iterator-var :reader iterator-var
                 :initform (gensym))
   (successp-var :reader successp-var
                 :initform (gensym))
   (next-key-var :accessor next-key-var
                 :initform (gensym))
   (next-value-var :accessor next-value-var
              :initform (gensym))))

(defmethod declaration-targets ((clause hash-table-clause))
  (bindings-from-d-var-spec (var-spec clause)))

(defmethod make-clause ((type (eql :each-key-value)) &rest initargs)
  (apply #'make-instance 'hash-table-clause :var-spec initargs))

(defmethod wrap-outer-form ((clause hash-table-clause) form)
  `(with-hash-table-iterator (,(iterator-var clause) ,(in-form clause))
     (multiple-value-bind (,(successp-var clause)
                           ,(next-key-var clause)
                           ,(next-value-var clause))
         (,(iterator-var clause))
       ,form)))

(defmethod wrap-inner-form ((clause hash-table-clause) form)
  `(let (,.(bindings-from-d-var-spec (first (var-spec clause))
                                     (next-key-var clause))
         ,.(bindings-from-d-var-spec (second (var-spec clause))
                                     (next-value-var clause)))
     ,.(apply #'declarations
              (bindings-from-d-var-spec (var-spec clause)))
     ,form))

(defmethod initial-early-forms ((clause hash-table-clause))
  `((unless ,(successp-var clause)
      (hoop-finish))))

(defmethod next-early-forms ((clause hash-table-clause))
  `((setf (values ,(successp-var clause)
                  ,(next-key-var clause)
                  ,(next-value-var clause))
          (,(iterator-var clause)))
    (unless ,(successp-var clause)
      (hoop-finish))))

(defmethod next-late-forms ((clause hash-table-clause))
  `((setq ,.(apply #'nconc (bindings-from-d-var-spec (first (var-spec clause))
                                                     (next-key-var clause)))
          ,.(apply #'nconc (bindings-from-d-var-spec (second (var-spec clause))
                                                     (next-value-var clause))))))

