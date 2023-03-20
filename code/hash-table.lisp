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
              :initform nil)))

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

(defmethod wrap-form ((clause hash-table-clause) form)
  `(with-hash-table-iterator (,(iterator-var clause) ,(in-form clause))
     (let (,(or (key-var clause) (first (var-spec clause)))
           ,(or (value-var clause) (second (var-spec clause)))
           ,(successp-var clause))
       ,(if (or (key-var clause)
                (value-var clause))
            `(symbol-macrolet (,.(when (key-var clause)
                                   (symbol-macros-from-d-var-spec (first (var-spec clause))
                                                                  (key-var clause)))
                                 ,.(when (value-var clause)
                                     (symbol-macros-from-d-var-spec (second (var-spec clause))
                                                                    (value-var clause))))
               ,form)
            form))))

(defmethod prologue-forms ((clause hash-table-clause))
  `((multiple-value-setq (,(successp-var clause)
                          ,(or (key-var clause) (first (var-spec clause)))
                          ,(or (value-var clause) (second (var-spec clause))))
      (,(iterator-var clause)))
    (unless ,(successp-var clause)
      (hoop-finish))))
