(in-package #:hoop)

(defclass step-clause (clause var-spec-slot from-form-slot by-slots)
  ((next-var :reader next-var
             :initform (gensym)))
  (:default-initargs :by 1
                     :from 0))

(defclass to-step-clause (step-clause)
  ((to :reader to
       :initarg :to)
   (to-var :reader to-var
           :initform (gensym))))

(defclass before-step-clause (step-clause)
  ((before :reader before
       :initarg :before)
   (before-var :reader before-var
           :initform (gensym))))

(defmethod make-clause ((keyword (eql :step)) &rest initargs)
  (apply #'make-instance (cond ((getf (cdr initargs) :to)
                                'to-step-clause)
                               ((getf (cdr initargs) :before)
                                'before-step-clause)
                               (t
                                'step-clause))
         :var-spec initargs))

(defmethod wrap-form ((clause step-clause) form)
  `(let (,(var-spec clause)
         ,@(assemble-in-order clause
                              `(:from ((,(next-var clause) ,(from-form clause)))
                                :by ((,(by-var clause) ,(by-form clause))))))
     ,form))

(defmethod wrap-form ((clause to-step-clause) form)
  `(let (,(var-spec clause)
         ,@(assemble-in-order clause
                              `(:from ((,(next-var clause) ,(from-form clause)))
                                :by ((,(by-var clause) ,(by-form clause)))
                                :to ((,(to-var clause) ,(to clause))))))
     ,form))

(defmethod wrap-form ((clause before-step-clause) form)
  `(let (,(var-spec clause)
         ,@(assemble-in-order clause
                              `(:from ((,(next-var clause) ,(from-form clause)))
                                :by ((,(by-var clause) ,(by-form clause)))
                                :before ((,(before-var clause) ,(before clause))))))
     ,form))

(defmethod termination-forms ((clause to-step-clause))
  `((unless (or (and (plusp ,(by-var clause))
                            (<= ,(next-var clause) ,(to-var clause)))
                       (and (minusp ,(by-var clause))
                            (>= ,(next-var clause) ,(to-var clause))))
      (hoop-finish))))

(defmethod termination-forms ((clause before-step-clause))
  `((unless (or (and (plusp ,(by-var clause))
                     (< ,(next-var clause) ,(before-var clause)))
                (and (minusp ,(by-var clause))
                     (> ,(next-var clause) ,(before-var clause))))
      (hoop-finish))))

(defmethod before-forms ((clause step-clause))
  `((setq ,(var-spec clause) ,(next-var clause))))

(defmethod after-forms ((clause step-clause))
  `((incf ,(next-var clause) ,(by-var clause))))
