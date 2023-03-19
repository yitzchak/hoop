(in-package #:hoop)

(defclass step-clause (var-spec-slot from-form-slot by-form-slot)
  ((to :reader to
       :initarg :to)
   (before :reader before
       :initarg :before)
   (by-var :reader by-var
           :initform (gensym))
   (to-var :reader to-var
           :initform (gensym)))
  (:default-initargs :by 1
                     :from 0))

(defmethod make-clause ((keyword (eql :step)) &rest initargs)
  (apply #'make-instance 'step-clause :var-spec initargs))

(defmethod bindings ((clause step-clause))  
  `((,(var-spec clause) ,(from-form clause))
    (,(by-var clause) ,(by-form clause))
    ,@(when (slot-boundp clause 'to)
        `((,(to-var clause) ,(to clause))))
    ,@(when (slot-boundp clause 'before)
        `((,(to-var clause) ,(before clause))))))

(defmethod prologue-forms ((clause step-clause))
  (cond ((slot-boundp clause 'to)
         `((unless (or (and (plusp ,(by-var clause))
                            (<= ,(var-spec clause) ,(to-var clause)))
                       (and (minusp ,(by-var clause))
                            (>= ,(var-spec clause) ,(to-var clause))))
             (hoop-finish))))
        ((slot-boundp clause 'before)
         `((unless (or (and (plusp ,(by-var clause))
                            (< ,(var-spec clause) ,(to-var clause)))
                       (and (minusp ,(by-var clause))
                            (> ,(var-spec clause) ,(to-var clause))))
             (hoop-finish))))))
        
(defmethod epilogue-forms ((clause step-clause))
  `((incf ,(var-spec clause) ,(by-var clause))))
