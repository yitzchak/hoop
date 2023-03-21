(in-package #:hoop)

(defclass step-clause (var-spec-slot from-form-slot by-slots binding-order-slot)
  ((next-var :reader next-var
             :initform (gensym))
   (to :reader to
       :initarg :to)
   (before :reader before
       :initarg :before)
   (to-var :reader to-var
           :initform (gensym)))
  (:default-initargs :by 1
                     :from 0))

(defmethod initialize-instance :after ((instance step-clause) &rest initargs &key)
  (setf (binding-order instance)
        (calculate-binding-order initargs :from :by :to :before)))

(defmethod make-clause ((keyword (eql :step)) &rest initargs)
  (apply #'make-instance 'step-clause :var-spec initargs))

(defmethod wrap-form ((clause step-clause) form)
  `(let (,(var-spec clause)
         ,@(make-bindings (binding-order clause)
                          `(:from (,(next-var clause) ,(from-form clause))
                            :by (,(by-var clause) ,(by-form clause))
                            :to (,(to-var clause) ,(when (slot-boundp clause 'to)
                                                     (to clause)))
                            :before (,(to-var clause) ,(when (slot-boundp clause 'before)
                                                         (before clause))))))
     ,form))

(defmethod termination-forms ((clause step-clause))
  (cond ((slot-boundp clause 'to)
         `((unless (or (and (plusp ,(by-var clause))
                            (<= ,(next-var clause) ,(to-var clause)))
                       (and (minusp ,(by-var clause))
                            (>= ,(next-var clause) ,(to-var clause))))
             (hoop-finish))))
        ((slot-boundp clause 'before)
         `((unless (or (and (plusp ,(by-var clause))
                            (< ,(next-var clause) ,(to-var clause)))
                       (and (minusp ,(by-var clause))
                            (> ,(next-var clause) ,(to-var clause))))
             (hoop-finish))))))

(defmethod before-forms ((clause step-clause))
  `((setq ,(var-spec clause) ,(next-var clause))))

(defmethod after-forms ((clause step-clause))
  `((incf ,(next-var clause) ,(by-var clause))))
