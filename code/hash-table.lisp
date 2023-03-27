(in-package #:hoop)

(defclass hash-table-clause (var-spec-slot in-form-slot temp-var-slot)
  ((iterator-var :reader iterator-var
                 :initform (gensym))
   (successp-var :reader successp-var
                 :initform (gensym))
   (key-var :accessor key-var
            :initarg :key-var
            :initform nil)
   (value-var :accessor value-var
              :initarg :value-var
              :initform nil)
   (next-key-var :accessor next-key-var
            :initform (gensym))
   (next-value-var :accessor next-value-var
              :initform (gensym))))

(defmethod initialize-instance :after ((instance hash-table-clause) &rest initargs &key)
  (declare (ignore initargs))
  (check-type (var-spec instance)
              (cons (or symbol cons) (cons (or symbol cons) null)))
  (when (listp (first (var-spec instance)))
    (setf (key-var instance) (gensym)))
  (when (listp (second (var-spec instance)))
    (setf (value-var instance) (gensym))))

(defmethod make-clause ((type (eql :each-key-value)) &rest initargs)
  (apply #'make-instance 'hash-table-clause :var-spec initargs))

(defmethod wrap-outer-form ((clause hash-table-clause) form)
  `(with-hash-table-iterator (,(iterator-var clause) ,(in-form clause))
     ,form))

(defmethod wrap-inner-form ((clause hash-table-clause) form)
  `(let (,(successp-var clause)
         ,(next-key-var clause)
         ,(next-value-var clause)
         ,.(mapcar #'first (bindings-from-d-var-spec (var-spec clause))))
     (multiple-value-setq (,(successp-var clause)
                           ,(next-key-var clause)
                           ,(next-value-var clause))
                          (,(iterator-var clause)))
     ,form))

(defmethod initial-early-forms ((clause hash-table-clause))
  `((unless ,(successp-var clause)
      (hoop-finish))))

(defmethod initial-late-forms ((clause hash-table-clause))
  `((setq ,.(apply #'nconc (bindings-from-d-var-spec (first (var-spec clause))
                                                     (next-key-var clause)))
          ,.(apply #'nconc (bindings-from-d-var-spec (second (var-spec clause))
                                                     (next-value-var clause))))))

(defmethod next-early-forms ((clause hash-table-clause))
  `((multiple-value-setq (,(successp-var clause)
                          ,(next-key-var clause)
                          ,(next-value-var clause))
      (,(iterator-var clause)))
    (unless ,(successp-var clause)
      (hoop-finish))))

(defmethod next-late-forms ((clause hash-table-clause))
  `((setq ,.(apply #'nconc (bindings-from-d-var-spec (first (var-spec clause))
                                                     (next-key-var clause)))
          ,.(apply #'nconc (bindings-from-d-var-spec (second (var-spec clause))
                                                     (next-value-var clause))))))

