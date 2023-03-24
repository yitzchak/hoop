(in-package #:hoop)

(defclass generate-clause (var-spec-slot using-form-slot temp-var-slot)
  ())

(defclass generate-then-clause (generate-clause)
  ((then-form :accessor then-form
              :initarg :then)
   (use-then-var :accessor use-then-var
                 :initform (gensym))))

(defmethod make-clause ((type (eql :generate)) &rest initargs)
  (apply #'make-instance (if (get-properties (cdr initargs) '(:then))
                             'generate-then-clause
                             'generate-clause)
         :var-spec initargs))

(defmethod wrap-form ((clause generate-clause) form)
  (if (listp (var-spec clause))
      `(let (,(temp-var clause)
             ,.(mapcar #'first (bindings-from-d-var-spec (var-spec clause))))
         ,form)
      `(let (,(var-spec clause))
         ,form)))

(defmethod wrap-form ((clause generate-then-clause) form)
  (if (listp (var-spec clause))
      `(let (,(use-then-var clause)
             ,(temp-var clause)
             ,.(mapcar #'first (bindings-from-d-var-spec (var-spec clause))))
         ,form)
      `(let (,(use-then-var clause)
             ,(var-spec clause))
         ,form)))

(defmethod before-forms ((clause generate-clause))
  (if (listp (var-spec clause))
      `((setq ,(temp-var clause) (multiple-value-list ,(using-form clause))
              ,.(apply #'nconc (bindings-from-d-var-spec (var-spec clause)
                                                         (temp-var clause)))))
      `((setq ,(var-spec clause) ,(using-form clause)))))

(defmethod before-forms ((clause generate-then-clause))
  (if (listp (var-spec clause))
      `((setq ,(temp-var clause) (if ,(use-then-var clause)
                                     (multiple-value-list ,(then-form clause))
                                     (multiple-value-list ,(using-form clause)))
              ,.(apply #'nconc (bindings-from-d-var-spec (var-spec clause)
                                                         (temp-var clause)))
              ,(use-then-var clause) t))
      `((setq ,(var-spec clause) (if ,(use-then-var clause)
                                     ,(then-form clause)
                                     ,(using-form clause))
              ,(use-then-var clause) t))))
