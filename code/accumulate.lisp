(in-package #:hoop)

(defclass collect-clause (var-spec-slot into-form-slot)
  ((tail-var :reader tail-var
             :initform (gensym )))
  (:default-initargs :into nil))

(defmethod make-clause ((keyword (eql :collect)) &rest initargs)
  (apply #'make-instance 'collect-clause :var-spec initargs))

(defmethod bindings ((clause collect-clause))  
  `((,(var-spec clause) ,(into-form clause))
    (,(tail-var clause) (last ,(var-spec clause)))))

(defmethod wrap-inner ((clause collect-clause) form)
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

(defmethod return-form ((clause collect-clause))
  (var-spec clause))
