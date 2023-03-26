(in-package #:hoop)

(defclass sequence-clause (clause var-spec-slot in-form-slot by-slots)
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

(defmethod make-clause (parallel (type (eql :each-elt)) &rest initargs)
  (apply #'make-instance 'sequence-clause :var-spec initargs))

(defmethod wrap-outer-form ((clause sequence-clause) form)
  `(let (,(index clause)
         ,@(assemble-in-order clause
                              `(:in ((,(seq-var clause) ,(in-form clause)))
                                :start ((,(next-index clause) ,(start clause)))
                                :by ((,(by-var clause) ,(by-form clause)))
                                :end ((,(end-var clause) ,(end clause))))))
     (symbol-macrolet ,(bindings-from-d-var-spec (var-spec clause)
                                                 `(elt ,(seq-var clause) ,(index clause)))
       ,form)))

(defmethod termination-forms ((clause sequence-clause))
  (if (end clause)
      `((unless (< ,(next-index clause) ,(end-var clause))
          (hoop-finish)))
      `((unless (< ,(next-index clause) (length ,(seq-var clause)))
          (hoop-finish)))))

(defmethod before-forms ((clause sequence-clause))
  `((setq ,(index clause) ,(next-index clause))))

(defmethod after-forms ((clause sequence-clause))
  `((incf ,(next-index clause) ,(by-var clause))))

