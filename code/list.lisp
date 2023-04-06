(in-package #:hoop)

(defclass list-clause (clause var-spec-slot by-slots in-form-slot update-slot)
  ((list-var :reader list-var
             :initform (gensym))
   (next-list-var :reader next-list-var
                  :initform (gensym)))
  (:default-initargs :by '(function cdr)))

(defclass list-item-clause (list-clause)
  ())

(defclass list-sublist-clause (list-clause on-form-slot)
  ())

(defmethod make-clause ((keyword (eql :each-item)) &rest initargs)
  (apply #'make-instance 'list-item-clause
         :var-spec initargs))

(defmethod make-clause ((keyword (eql :each-sublist)) &rest initargs)
  (apply #'make-instance 'list-sublist-clause
         :var-spec initargs))

(defmethod declaration-targets ((clause list-clause))
  (unless (update clause)
    (variable-names (var-spec clause))))

(defmethod wrap-outer-form ((clause list-clause) form)
  `(let (,.(when (update clause)
             (list (list-var clause)))
         ,@(assemble-in-order clause
                              `(:in ((,(next-list-var clause) ,(in-form clause)))
                                :by ((,(by-var clause) ,(by-form clause))))))
     ,form))

(defmethod wrap-inner-form ((clause list-item-clause) form)
  (if (update clause)
      `(symbol-macrolet ,(bindings-from-d-var-spec (var-spec clause)
                                                   `(car ,(list-var clause)))
         ,form)
      `(let ,(bindings-from-d-var-spec (var-spec clause)
                                       `(car ,(next-list-var clause)))
         ,.(declarations (variable-names (var-spec clause)))
        ,form)))

(defmethod wrap-inner-form ((clause list-sublist-clause) form)
  (if (update clause)
      `(symbol-macrolet ,(bindings-from-d-var-spec (var-spec clause)
                                                   (list-var clause))
         ,form)
      `(let ,(bindings-from-d-var-spec (var-spec clause)
                                       (next-list-var clause))
         ,.(declarations (variable-names (var-spec clause)))
         ,form)))

(defmethod initial-early-forms ((clause list-clause))
  `((when (endp ,(next-list-var clause))
      (hoop-finish))))

(defmethod initial-early-forms ((clause list-sublist-clause))
  `((when (atom ,(next-list-var clause))
      (hoop-finish))))

(defmethod initial-late-forms ((clause list-clause))
  (when (update clause)
    `((setq ,(list-var clause) ,(next-list-var clause)))))

(defmethod next-early-forms ((clause list-item-clause))
  `((setq ,(next-list-var clause) (funcall ,(by-var clause) ,(next-list-var clause)))
    (when (endp ,(next-list-var clause))
      (hoop-finish))))
  
(defmethod next-early-forms ((clause list-sublist-clause))
  `((setq ,(next-list-var clause) (funcall ,(by-var clause) ,(next-list-var clause)))
    (when (atom ,(next-list-var clause))
      (hoop-finish))))
  
(defmethod next-late-forms ((clause list-item-clause))
  (if (update clause)
      `((setq ,(list-var clause) ,(next-list-var clause)))
      `((setq ,.(assignments-from-d-var-spec (var-spec clause)
                                             `(car ,(next-list-var clause)))))))

(defmethod next-late-forms ((clause list-sublist-clause))
  (if (update clause)
      `((setq ,(list-var clause) ,(next-list-var clause)))
      `((setq ,.(assignments-from-d-var-spec (var-spec clause)
                                             (next-list-var clause))))))
