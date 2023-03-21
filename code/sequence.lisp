(in-package #:hoop)

(defclass sequence-clause (var-spec-slot in-form-slot by-slots)
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
          :initarg :start
          :initform 0)
   (end :reader end
        :initarg :end
        :initform nil))
  (:default-initargs :by 1))

(defmethod make-clause ((type (eql :each-elt)) &rest initargs)
  (apply #'make-instance 'sequence-clause :var-spec initargs))

(defmethod wrap-form ((clause sequence-clause) form)
  `(let ((,(seq-var clause) ,(in-form clause))
         (,(next-index-var clause) ,(start clause))
         ,(index clause)
         (,(by-var clause) ,(by-form clause))
         ,@(when (end clause)
             `((,(end-var clause) ,(end clause)))))
     (symbol-macrolet ,(symbol-macros-from-d-var-spec (var-spec clause) `(elt ,(seq-var clause) ,(index clause)))
       ,form)))

(defmethod termination-forms ((clause sequence-clause))
  (if (end clause)
      `((unless (< ,(next-index-var clause) ,(end-var clause))
          (hoop-finish)))
      `((unless (< ,(next-index-var clause) (length ,(seq-var clause)))
          (hoop-finish)))))

(defmethod before-forms ((clause sequence-clause))
  `((setq ,(index clause) ,(next-index-var clause))))

(defmethod after-forms ((clause sequence-clause))
  `((incf ,(next-index-var clause) ,(by-var clause))))

