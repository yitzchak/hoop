(in-package #:hoop)

(defclass each-elt-clause (clause var-spec-slot in-form-slot by-slots update-slot)
  ((seq-var :reader seq-var
            :initform (gensym "SEQ"))
   (end-var :reader end-var
            :initform (gensym "END"))
   (index :reader index
          :initarg :index
          :initform (gensym "INDEX"))
   (next-index :reader next-index
               :initform (gensym "NEXT-INDEX-"))
   (start-form :reader start-form
               :initarg :start)
   (end-form :reader end-form
             :initarg :end
             :initform nil))
  (:default-initargs :start 0
                     :by 1))

(defmethod make-clause ((type (eql :each-elt)) &rest initargs)
  (apply #'make-instance 'each-elt-clause :var-spec initargs))

(defmethod declaration-targets ((clause each-elt-clause))
  (unless (update clause)
    (variable-names (var-spec clause))))

(defmethod wrap-outer-form ((clause each-elt-clause) form)
  `(let (,(index clause)
         ,@(assemble-in-order clause
                              `(:in ((,(seq-var clause) ,(in-form clause)))
                                :start ((,(next-index clause) ,(start-form clause)))
                                :by ((,(by-var clause) ,(by-form clause)))
                                :end ((,(end-var clause) ,(end-form clause))))))
     (when (< ,(next-index clause) ,(if (end-form clause)
                                        (end-var clause)
                                        `(length ,(seq-var clause))))
       (setq ,(index clause) ,(next-index clause)))
     ,form))

(defmethod wrap-inner-form ((clause each-elt-clause) form)
  (if (update clause)
      `(symbol-macrolet ,(bindings-from-d-var-spec (var-spec clause)
                                                   `(elt ,(seq-var clause)
                                                         ,(index clause)))
         ,form)
      `(let ,(bindings-from-d-var-spec (var-spec clause)
                                       `(when ,(index clause)
                                          (elt
                                           ,(seq-var clause) ,(index clause))))
         ,.(declarations (variable-names (var-spec clause)))
         ,form)))

(defmethod initial-early-forms ((clause each-elt-clause))
  `((unless (< ,(next-index clause) ,(if (end-form clause)
                                         (end-var clause)
                                         `(length ,(seq-var clause))))
      (hoop-finish))))

(defmethod next-early-forms ((clause each-elt-clause))
  `((incf ,(next-index clause) ,(by-var clause))
    (unless (< ,(next-index clause) ,(if (end-form clause)
                                         (end-var clause)
                                         `(length ,(seq-var clause))))
      (hoop-finish))))

(defmethod next-late-forms ((clause each-elt-clause))
  `((setq ,(index clause) ,(next-index clause)
          ,.(unless (update clause)
              (assignments-from-d-var-spec (var-spec clause)
                                           `(elt ,(seq-var clause)
                                                 ,(index clause)))))))
