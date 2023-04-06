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
     (multiple-value-bind (,(successp-var clause) ,(next-symbol-var clause)
                           ,(next-status-var clause) ,(next-package-var clause))
         (,(iterator-var clause))
       ,form)))

(defmethod wrap-inner-form ((clause package-clause) form)
  (if (or (var-spec clause)
          (status-var clause)
          (package-var clause))
      `(let (,.(when (var-spec clause)
                 `((,(var-spec clause) ,(next-symbol-var clause))))
             ,.(when (status-var clause)
                 `((,(status-var clause) ,(next-status-var clause))))
             ,.(when (package-var clause)
                 `((,(package-var clause) ,(next-package-var clause)))))
         ,.(apply #'declarations (nconc (when (var-spec clause)
                                          (list (var-spec clause)))
                                        (when (status-var clause)
                                          (list (status-var clause)))
                                        (when (package-var clause)
                                          (list (package-var clause)))))
         ,form)
      form))

(defmethod declaration-targets ((clause package-clause))
  (nconc (when (var-spec clause)
           (list (var-spec clause)))
         (when (status-var clause)
           (list (status-var clause)))
         (when (package-var clause)
           (list (package-var clause)))))

(defmethod initial-early-forms ((clause package-clause))
  `((unless ,(successp-var clause)
      (hoop-finish))))

(defmethod next-early-forms ((clause package-clause))
  `((setf (values ,(successp-var clause)
                  ,(next-symbol-var clause)
                  ,(next-status-var clause)
                  ,(next-package-var clause))
          (,(iterator-var clause)))
    (unless ,(successp-var clause)
      (hoop-finish))))

(defmethod next-late-forms ((clause package-clause))
  (nconc (when (var-spec clause)
           `((setq ,(var-spec clause) ,(next-symbol-var clause))))
         (when (status-var clause)
           `((setq ,(status-var clause) ,(next-status-var clause))))
         (when (package-var clause)
           `((setq ,(package-var clause) ,(next-package-var clause))))))
