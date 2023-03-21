(in-package #:hoop)

(defclass list-clause (var-spec-slot by-slots in-form-slot)
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

(defmethod wrap-form ((clause list-item-clause) form)
  `(let ((,(next-list-var clause) ,(in-form clause))
         (,(by-var clause) ,(by-form clause))
         ,(list-var clause))
     (symbol-macrolet ,(symbol-macros-from-d-var-spec (var-spec clause)
                                                      `(car ,(list-var clause)))
       ,form)))

(defmethod wrap-form ((clause list-sublist-clause) form)
  `(let ((,(next-list-var clause) ,(in-form clause))
         (,(by-var clause) ,(by-form clause))
         ,(list-var clause))
     (symbol-macrolet ,(symbol-macros-from-d-var-spec (var-spec clause)
                                                      (list-var clause))
       ,form)))

(defmethod termination-forms ((clause list-clause))
  `((when (endp ,(next-list-var clause))
      (hoop-finish))))
  
(defmethod before-forms ((clause list-clause))
  `((setq ,(list-var clause) ,(next-list-var clause))))

(defmethod after-forms ((clause list-clause))
  `((setq ,(next-list-var clause) (funcall ,(by-var clause) ,(next-list-var clause)))))

