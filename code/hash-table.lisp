(in-package #:hoop)

(defclass hash-table-clause (var-spec-slot initform-slot)
  ((iterator-var :reader iterator-var
                 :initform (gensym))
   (successp-var :reader successp-var
                 :initform (gensym))
   (key-var :reader key-var
            :initarg :key-var
            :initform nil)
   (value-var :reader value-var
              :initarg :value-var
              :initform nil)))

(defmethod initialize-instance :after ((instance hash-table-clause) &rest initargs &key)
  (declare (ignore initargs))
  (check-type (var-spec instance)
              (cons (or symbol cons) (cons (or symbol cons) null)))
  (when (listp (first (var-spec instance)))
    (setf (key-var instance) (gensym)))
  (when (listp (second (var-spec instance)))
    (setf (value-var instance) (gensym))))

(defmethod make-clause ((keyword (eql :in-hash-table)) &rest initargs)
  (apply #'make-instance 'hash-table-clause :var-spec initargs))

(defmethod wrap-outer ((clause hash-table-clause) form)
  `(with-hash-table-iterator (,(iterator-var clause) ,(initform clause))
     ,form))

(defmethod wrap-inner ((clause hash-table-clause) form)
  (if (or (key-var clause)
          (value-var clause))
      `(symbol-macrolet ,(nconc (when (key-var clause)
                                  (symbol-macros-from-d-var-spec (first (var-spec clause))
                                                                 (key-var clause)))
                                (when (value-var clause)
                                  (symbol-macros-from-d-var-spec (second (var-spec clause))
                                                                 (value-var clause))))
         ,form)
      form))

(defmethod bindings ((clause hash-table-clause))
  `(,(or (key-var clause) (first (var-spec clause)))
    ,(or (value-var clause) (second (var-spec clause)))
    ,(successp-var clause)))

(defmethod prologue-forms ((clause hash-table-clause))
  `((multiple-value-setq (,(successp-var clause)
                          ,(or (key-var clause) (first (var-spec clause)))
                          ,(or (value-var clause) (second (var-spec clause))))
      (,(iterator-var clause)))
    (unless ,(successp-var clause)
      (hoop-finish))))
