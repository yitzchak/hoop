(in-package #:hoop)

(defclass package-clause (clause)
  ((symbol-types :reader symbol-types
                 :initarg :symbol-types)
   (iterator-var :reader iterator-var
                 :initform (gensym))
   (successp-var :reader successp-var
                 :initform (gensym))
   (status-var :reader status-var
               :initarg :status-var
               :initform (gensym))
   (package-var :reader package-var
                :initarg :package-var
                :initform (gensym))))

(defmethod expand (var (action (eql :symbols)) &optional initform &rest initargs
                   &key (symbol-types '(:external))
                        (status (gensym))
                        (package (gensym))
                   &allow-other-keys)
  (declare (ignore initargs))
  (make-instance 'package-clause :var var :initform initform
                                 :symbol-types symbol-types
                                 :status-var status
                                 :package-var package))

(defmethod wrap-outer ((clause package-clause) form)
  `(with-package-iterator (,(iterator-var clause) ,(initform clause) ,@(symbol-types clause))
     ,form))

(defmethod bindings ((clause package-clause))
  `(,(var clause) ,(successp-var clause) ,(status-var clause) ,(package-var clause)))

(defmethod prologue ((clause package-clause))
  `((multiple-value-setq (,(successp-var clause) ,(var clause)
                          ,(status-var clause) ,(package-var clause))
      (,(iterator-var clause)))
    (unless ,(successp-var clause)
      (hoop-finish))))
