(in-package #:hoop)

(defclass sequence-clause (clause)
  ((seq-var :reader seq-var
            :initform (gensym))
   (by-var :reader by-var
           :initform (gensym))
   (end-var :reader end-var
            :initform (gensym))
   (index :reader index
          :initarg :index
          :initform (gensym))
   (by :reader by
       :initarg :by
       :initform 1)
   (start :reader start
          :initarg :start
          :initform 0)
   (end :reader end
        :initarg :end
        :initform nil)))

(defmethod expand (var (action (eql :in-seq)) &optional initform &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (apply #'make-instance 'sequence-clause :var var :initform initform initargs))

(defmethod bindings ((clause sequence-clause))
  `((,(seq-var clause) ,(initform clause))
    (,(index clause) ,(start clause))
    (,(by-var clause) ,(by clause))
    ,@(when (end clause)
        `((,(end-var clause) ,(end clause))))))

(defmethod wrap-inner ((clause sequence-clause) form)
  `(symbol-macrolet ,(symbol-macros-from-d-var-spec (var clause) `(elt ,(seq-var clause) ,(index clause)))
     ,form))

(defmethod prologue ((clause sequence-clause))
  (if (end clause)
      `((unless (< ,(index clause) ,(end-var clause))
          (hoop-finish)))
      `((unless (< ,(index clause) (length ,(seq-var clause)))
          (hoop-finish)))))

(defmethod epilogue ((clause sequence-clause))
  `((incf ,(index clause) ,(by-var clause))))

