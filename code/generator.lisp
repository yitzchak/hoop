(in-package #:hoop)

(defclass generator-clause (clause)
  ((then :reader then
         :initarg :then)
   (%while :reader %while
           :initarg :while)
   (temp-var :reader temp-var
             :initform (gensym))))

(defmethod expand (var (action (eql :generator)) &optional initform &rest initargs &key &allow-other-keys)
  (apply #'make-instance 'generator-clause
         :var var :initform initform initargs))

(defmethod bindings ((clause generator-clause))
  (if (listp (var clause))
      `((,(temp-var clause) (multiple-value-list ,(initform clause))))
      `((,(var clause) ,(initform clause)))))

(defmethod wrap-inner ((clause generator-clause) form)
  (if (listp (var clause))
      `(symbol-macrolet ,(symbol-macros-from-d-var-spec (var clause) (temp-var clause))
         ,form)
      form))

(defmethod prologue ((clause generator-clause))
  (when (slot-boundp clause '%while)
    `((unless ,(%while clause)
        (hoop-finish)))))

(defmethod epilogue ((clause generator-clause))
  (if (listp (var clause))
      `((setq ,(temp-var clause)
              (multiple-value-list ,(if (slot-boundp clause 'then)
                                        (then clause)
                                        (initform clause)))))      
      `((setq ,(var clause)
              ,(if (slot-boundp clause 'then)
                   (then clause)
                   (initform clause))))))
