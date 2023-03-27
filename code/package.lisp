(in-package #:hoop)

(defclass package-clause (var-spec-slot in-form-slot temp-var-slot)
  ((symbol-types :reader symbol-types
                 :initform '(:external)
                 :initarg :symbol-types)
   (iterator-var :reader iterator-var
                 :initform (gensym))
   (successp-var :reader successp-var
                 :initform (gensym))
   (next-symbol-var :reader next-symbol-var
                    :initform (gensym))
   (next-status-var :reader next-status-var
                    :initform (gensym))
   (status-var :reader status-var
               :initarg :status
               :initform nil)
   (next-package-var :reader next-package-var
                     :initform (gensym))
   (package-var :reader package-var
                :initarg :package
                :initform nil))
  (:default-initargs :in '*package*))

(defmethod make-clause (parallel (type (eql :each-symbol)) &rest initargs)
  (apply #'make-instance 'package-clause :var-spec initargs))

(defmethod wrap-outer-form ((clause package-clause) form)
  `(with-package-iterator (,(iterator-var clause) ,(in-form clause) ,@(symbol-types clause))
     ,form))

(defmethod wrap-inner-form ((clause package-clause) form)
  `(let (,.(when (var-spec clause)
             (list (var-spec clause)))
         ,(successp-var clause)
         ,(next-symbol-var clause)
         ,(next-status-var clause)
         ,(next-package-var clause)
         ,.(when (status-var clause)
             (list (status-var clause)))
         ,.(when (package-var clause)
             (list (package-var clause))))
     (multiple-value-setq (,(successp-var clause)
                           ,(next-symbol-var clause)
                           ,(next-status-var clause)
                           ,(next-package-var clause))
       (,(iterator-var clause)))
     ,form))

(defmethod termination-forms ((clause package-clause))
  `((unless ,(successp-var clause)
      (hoop-finish))))

(defmethod before-forms ((clause package-clause))
  `((setq ,@(when (var-spec clause)
              (list (var-spec clause) (next-symbol-var clause)))
          ,@(when (status-var clause)
              (list (status-var clause) (next-status-var clause)))
          ,@(when (package-var clause)
              (list (package-var clause) (next-package-var clause))))))

(defmethod after-forms ((clause package-clause))
  `((multiple-value-setq (,(successp-var clause)
                          ,(next-symbol-var clause)
                          ,(next-status-var clause)
                          ,(next-package-var clause))
      (,(iterator-var clause)))))
