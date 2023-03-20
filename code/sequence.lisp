(in-package #:hoop)

(defclass sequence-clause (var-spec-slot in-form-slot by-slots)
  ((seq-var :reader seq-var
            :initform (gensym))
   (end-var :reader end-var
            :initform (gensym))
   (index :reader index
          :initarg :index
          :initform (gensym))
   (start :reader start
          :initarg :start
          :initform 0)
   (end :reader end
        :initarg :end
        :initform nil)))

(defmethod make-clause ((type (eql :each-elt)) &rest initargs)
  (apply #'make-instance 'sequence-clause :var-spec initargs))

(defmethod bindings ((clause sequence-clause))
  `((,(seq-var clause) ,(in-form clause))
    (,(index clause) ,(start clause))
    (,(by-var clause) ,(by-form clause))
    ,@(when (end clause)
        `((,(end-var clause) ,(end clause))))))

(defmethod wrap-form ((clause sequence-clause) form)
  `(symbol-macrolet ,(symbol-macros-from-d-var-spec (var-spec clause) `(elt ,(seq-var clause) ,(index clause)))
     ,form))

(defmethod prologue-forms ((clause sequence-clause))
  (if (end clause)
      `((unless (< ,(index clause) ,(end-var clause))
          (hoop-finish)))
      `((unless (< ,(index clause) (length ,(seq-var clause)))
          (hoop-finish)))))

(defmethod epilogue-forms ((clause sequence-clause))
  `((incf ,(index clause) ,(by-var clause))))

