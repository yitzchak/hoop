(in-package #:hoop)

(defclass generator-clause (clause)
  ((then :reader then
         :initarg :then)
   (%while :reader %while
           :initarg :while)))

(defmethod expand (var (action (eql :generator)) &optional initform &rest initargs &key &allow-other-keys)
  (apply #'make-instance 'generator-clause
         :var var :initform initform initargs))

(defmethod bindings ((clause generator-clause))  
  `((,(var clause) ,(initform clause))))

(defmethod prologue ((clause generator-clause))
  (when (slot-boundp clause '%while)
    `((unless ,(%while clause)
        (hoop-finish)))))

(defmethod epilogue ((clause generator-clause))
  `((setq ,(var clause) ,(if (slot-boundp clause 'then)
                             (then clause)
                             (initform clause)))))
