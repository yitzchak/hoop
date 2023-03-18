(in-package #:hoop)

(defclass arithmetic-clause (clause)
  ((by :reader by
       :initarg :by
       :initform 1)
   (to :reader to
       :initarg :to
       :initarg :before
       :initform nil)
   (equality :reader equality
             :initarg :equality
             :initform nil)
   (by-var :reader by-var
           :initform (gensym))
   (to-var :reader to-var
           :initform (gensym))))

(defmethod expand (var (action (eql :from)) &optional initform &rest initargs &key to &allow-other-keys)
  (apply #'make-instance 'arithmetic-clause
         :var var :initform initform :equality (and to t) initargs))

(defmethod bindings ((clause arithmetic-clause))  
  `((,(var clause) ,(initform clause))
    (,(by-var clause) ,(by clause))
    ,@(when (to clause)
        `((,(to-var clause) ,(to clause))))))

(defmethod prologue ((clause arithmetic-clause))
  (when (to clause)
    `((unless (or (and (plusp ,(by-var clause))
                       (,(if (equality clause) '<= '<) ,(var clause) ,(to-var clause)))
                  (and (minusp ,(by-var clause))
                       (,(if (equality clause) '>= '>) ,(var clause) ,(to-var clause))))
        (hoop-finish)))))

(defmethod epilogue ((clause arithmetic-clause))
  `((incf ,(var clause) ,(by-var clause))))
