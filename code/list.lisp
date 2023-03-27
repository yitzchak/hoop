(in-package #:hoop)

(defclass list-clause (clause var-spec-slot by-slots in-form-slot)
  ((list-var :reader list-var
             :initform (gensym))
   (next-list-var :reader next-list-var
                  :initform (gensym)))
  (:default-initargs :by '(function cdr)))

(defclass list-item-clause (list-clause)
  ())

(defclass list-sublist-clause (list-clause on-form-slot)
  ())

(defmethod make-clause (parallel (keyword (eql :each-item)) &rest initargs)
  (apply #'make-instance 'list-item-clause
         :var-spec initargs))

(defmethod make-clause (parallel (keyword (eql :each-sublist)) &rest initargs)
  (apply #'make-instance 'list-sublist-clause
         :var-spec initargs))

(defmethod wrap-outer-form ((clause list-clause) form)
  `(let* (,(list-var clause)
         ,@(assemble-in-order clause
                              `(:in ((,(next-list-var clause) ,(in-form clause)))
                                :by ((,(by-var clause) ,(by-form clause))))))
     ,form))

(defmethod wrap-inner-form ((clause list-item-clause) form)
  `(symbol-macrolet ,(bindings-from-d-var-spec (var-spec clause)
                                               `(car ,(list-var clause)))
     ,form))

(defmethod wrap-inner-form ((clause list-sublist-clause) form)
  `(symbol-macrolet ,(bindings-from-d-var-spec (var-spec clause)
                                               (list-var clause))
     ,form))

(defmethod termination-forms ((clause list-clause))
  `((when (endp ,(next-list-var clause))
      (hoop-finish))))
  
(defmethod before-forms ((clause list-clause))
  `((setq ,(list-var clause) ,(next-list-var clause))))

(defmethod after-forms ((clause list-clause))
  `((setq ,(next-list-var clause) (funcall ,(by-var clause) ,(next-list-var clause)))))

