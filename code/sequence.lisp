(in-package #:hoop)

(defclass sequence-clause (clause var-spec-slot in-form-slot by-slots update-slot)
  ((seq-var :reader seq-var
            :initform (gensym))
   (end-var :reader end-var
            :initform (gensym))
   (index :reader index
          :initarg :index
          :initform (gensym))
   (next-index :reader next-index
               :initform (gensym))
   (start :reader start
          :initarg :start)
   (end :reader end
        :initarg :end
        :initform nil))
  (:default-initargs :start 0
                     :by 1))

(defmethod make-clause ((type (eql :each-elt)) &rest initargs)
  (apply #'make-instance 'sequence-clause :var-spec initargs))

(defmethod declaration-targets ((clause sequence-clause))
  (unless (update clause)
    (bindings-from-d-var-spec (var-spec clause))))

(defmethod wrap-outer-form ((clause sequence-clause) form)
  `(let (,(index clause)
         ,@(assemble-in-order clause
                              `(:in ((,(seq-var clause) ,(in-form clause)))
                                :start ((,(next-index clause) ,(start clause)))
                                :by ((,(by-var clause) ,(by-form clause)))
                                :end ((,(end-var clause) ,(end clause))))))
     (when (< ,(next-index clause) ,(if (end clause)
                                        (end-var clause)
                                        `(length ,(seq-var clause))))
       (setq ,(index clause) ,(next-index clause)))
     ,form))

(defmethod wrap-inner-form ((clause sequence-clause) form)
  (if (update clause)
      `(symbol-macrolet ,(bindings-from-d-var-spec (var-spec clause)
                                                   `(elt ,(seq-var clause) ,(index clause)))
         ,form)
      `(let ,(bindings-from-d-var-spec (var-spec clause)
                                       `(when ,(index clause)
                                          (elt ,(seq-var clause) ,(index clause))))
         ,.(apply #'declarations
                  (bindings-from-d-var-spec (var-spec clause)))
         ,form)))

(defmethod initial-early-forms ((clause sequence-clause))
  `((unless (< ,(next-index clause) ,(if (end clause)
                                         (end-var clause)
                                         `(length ,(seq-var clause))))
      (hoop-finish))))

(defmethod next-early-forms ((clause sequence-clause))
  `((incf ,(next-index clause) ,(by-var clause))
    (unless (< ,(next-index clause) ,(if (end clause)
                                         (end-var clause)
                                         `(length ,(seq-var clause))))
      (hoop-finish))))

(defmethod next-late-forms ((clause sequence-clause))
  `((setq ,(index clause) ,(next-index clause)
          ,.(unless (update clause)
              (apply #'nconc
                     (bindings-from-d-var-spec (var-spec clause)
                                               `(elt ,(seq-var clause) ,(index clause))))))))

