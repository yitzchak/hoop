(in-package #:hoop)

(defclass collect-clause (var-spec-slot into-form-slot)
  ((tail-var :reader tail-var
             :initform (gensym)))
  (:default-initargs :into nil))

(defclass parallel-collect-clause (collect-clause temp-var-slot)
  ())

(defmethod make-clause (parallel (type (eql :collect)) &rest initargs)
  (apply #'make-instance (if parallel
                             'parallel-collect-clause
                             'collect-clause)
         :var-spec initargs))

(defmethod wrap-outer-form ((clause collect-clause) form)
  `(let* ((,(var-spec clause) ,(into-form clause))
          (,(tail-var clause) (last ,(var-spec clause))))
     ,form))

(defmethod wrap-outer-form ((clause parallel-collect-clause) form)
  `(let* ((,(temp-var clause) ,(into-form clause))
          (,(tail-var clause) (last ,(temp-var clause))))
     ,form))

(defmethod wrap-inner-form ((clause collect-clause) form)
  `(flet ((,(var-spec clause) (x &optional (method :collect))
            (check-type method
                        (member :collect :append :nconc))
            (unless (eq method :collect)
              (check-type x list))
            (if ,(tail-var clause)
                (case method
                  (:append
                   (setf (cdr ,(tail-var clause)) (copy-list x)
                         ,(tail-var clause) (last ,(tail-var clause))))
                  (:nconc
                   (setf (cdr ,(tail-var clause)) x
                         ,(tail-var clause) (last ,(tail-var clause))))
                  (otherwise
                   (setf (cdr ,(tail-var clause)) (list x)
                         ,(tail-var clause) (cdr ,(tail-var clause)))))
                (case method
                  (:append
                   (setf ,(var-spec clause) (copy-list x)
                         ,(tail-var clause) (last ,(var-spec clause))))
                  (:nconc
                   (setf ,(var-spec clause) x
                         ,(tail-var clause) (last ,(var-spec clause))))
                  (otherwise
                   (setf ,(var-spec clause) (list x)
                         ,(tail-var clause) ,(var-spec clause)))))
            x))
     ,form))

(defmethod wrap-inner-form :around ((clause parallel-collect-clause) form)
  `(let* ((,(var-spec clause) ,(temp-var clause)))
     ,(call-next-method)))

(defmethod return-form ((clause collect-clause))
  (values t (var-spec clause)))
