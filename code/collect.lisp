(in-package #:hoop)

(defclass collect-clause (clause)
  ((tail-var :reader tail-var
             :initform (gensym ))))

(defmethod expand (var (action (eql :collect)) initform &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (make-instance 'collect-clause :var var :initform initform))

(defmethod bindings ((clause collect-clause))  
  `((,(var clause) ,(initform clause))
    (,(tail-var clause) (last ,(var clause)))))

(defmethod wrap-inner ((clause collect-clause) form)
  `(flet ((,(var clause) (x &optional method)
            (check-type method
                        (member nil :append :nconc))
            (when method
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
                   (setf ,(var clause) (copy-list x)
                         ,(tail-var clause) (last ,(var clause))))
                  (:nconc
                   (setf ,(var clause) x
                         ,(tail-var clause) (last ,(var clause))))
                  (otherwise
                   (setf ,(var clause) (list x)
                         ,(tail-var clause) ,(var clause)))))
            x))
     ,form))
