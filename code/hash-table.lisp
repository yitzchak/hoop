(in-package #:hoop)

(defclass hash-table-clause (clause)
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

(defmethod expand (var (action (eql :in-hash-table)) &optional initform &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (check-type var (cons (or symbol cons) (cons (or symbol cons) null)))
  (make-instance 'hash-table-clause
                 :var var :initform initform
                 :key-var (when (listp (first var))
                            (gensym))
                 :value-var (when (listp (second var))
                              (gensym))))

(defmethod wrap-outer ((clause hash-table-clause) form)
  `(with-hash-table-iterator (,(iterator-var clause) ,(initform clause))
     ,form))

(defmethod wrap-inner ((clause hash-table-clause) form)
  (if (or (key-var clause)
          (value-var clause))
      `(symbol-macrolet ,(nconc (when (key-var clause)
                                  (symbol-macros-from-d-var-spec (first (var clause))
                                                                 (key-var clause)))
                                (when (value-var clause)
                                  (symbol-macros-from-d-var-spec (second (var clause))
                                                                 (value-var clause))))
         ,form)
      form))

(defmethod bindings ((clause hash-table-clause))
  `(,(or (key-var clause) (first (var clause)))
    ,(or (value-var clause) (second (var clause)))
    ,(successp-var clause)))

(defmethod declarations ((clause hash-table-clause))
  (cond ((and (key-var clause) (value-var clause))
         `((declare (ignorable ,(key-var clause) ,(value-var clause)))))
        ((key-var clause)
         `((declare (ignorable ,(key-var clause)))))
        ((value-var clause)
         `((declare (ignorable ,(value-var clause)))))))

(defmethod prologue ((clause hash-table-clause))
  `((multiple-value-setq (,(successp-var clause)
                          ,(or (key-var clause) (first (var clause)))
                          ,(or (value-var clause) (second (var clause))))
      (,(iterator-var clause)))
    (unless ,(successp-var clause)
      (hoop-finish))))
