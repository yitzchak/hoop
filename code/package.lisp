(in-package #:hoop)

(defclass package-clause (var-spec-slot in-form-slot temp-var-slot)
  ((symbol-types :reader symbol-types
                 :initform '(:external)
                 :initarg :symbol-types)
   (iterator-var :reader iterator-var
                 :initform (gensym "PKG-ITER-"))
   (successp-var :reader successp-var
                 :initform (gensym "PKG-SUCCESSP-"))
   (next-symbol-var :reader next-symbol-var
                    :initform (gensym "PKG-NEXT-SYM-"))
   (next-status-var :reader next-status-var
                    :initform (gensym "PKG-NEXT-STATUS-"))
   (status-var :reader status-var
               :initarg :status
               :initform nil)
   (next-package-var :reader next-package-var
                     :initform (gensym "PKG-NEXT-PKG-"))
   (package-var :reader package-var
                :initarg :package
                :initform nil))
  (:default-initargs :in '*package*))

(defmethod make-clause ((type (eql :each-symbol)) &rest initargs)
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

(defmethod initial-early-forms ((clause package-clause))
  `((unless ,(successp-var clause)
      (hoop-finish))))

(defmethod initial-late-forms ((clause package-clause))
  `((setq ,@(when (var-spec clause)
              (list (var-spec clause) (next-symbol-var clause)))
          ,@(when (status-var clause)
              (list (status-var clause) (next-status-var clause)))
          ,@(when (package-var clause)
              (list (package-var clause) (next-package-var clause))))))

(defmethod next-early-forms ((clause package-clause))
  `((multiple-value-setq (,(successp-var clause)
                          ,(next-symbol-var clause)
                          ,(next-status-var clause)
                          ,(next-package-var clause))
      (,(iterator-var clause)))
    (unless ,(successp-var clause)
      (hoop-finish))))

(defmethod next-late-forms ((clause package-clause))
  `((setq ,@(when (var-spec clause)
              (list (var-spec clause) (next-symbol-var clause)))
          ,@(when (status-var clause)
              (list (status-var clause) (next-status-var clause)))
          ,@(when (package-var clause)
              (list (package-var clause) (next-package-var clause))))))
