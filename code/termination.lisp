(in-package #:hoop)

(defclass termination-clause ()
  ((test-form :reader test-form
              :initarg :test)))

(defclass termination-with-return-clause (termination-clause temp-var-slot)
  ())

(defmethod return-value-forms ((clause termination-with-return-clause))
  (list (temp-var clause)))

(defclass while-clause (termination-clause)
  ())

(defmethod make-clause ((type (eql :while)) &rest initargs)
  (apply #'make-instance 'while-clause :test initargs))

(defmethod initial-movable-forms ((clause while-clause))
  `((unless ,(test-form clause)
      (hoop-finish))))

(defmethod next-movable-forms ((clause while-clause))
  `((unless ,(test-form clause)
      (hoop-finish))))

(defclass until-clause (termination-clause)
  ((test-form :reader test-form
              :initarg :test)))

(defmethod make-clause ((type (eql :until)) &rest initargs)
  (apply #'make-instance 'until-clause :test initargs))

(defmethod initial-movable-forms ((clause until-clause))
  `((when ,(test-form clause)
      (hoop-finish))))

(defmethod next-movable-forms ((clause until-clause))
  `((when ,(test-form clause)
      (hoop-finish))))

(defclass always-clause (termination-with-return-clause)
  ())

(defmethod make-clause ((type (eql :always)) &rest initargs)
  (apply #'make-instance 'always-clause :test initargs))

(defmethod wrap-outer-form ((clause always-clause) form)
  `(let ((,(temp-var clause) t))
     ,form))

(defmethod initial-movable-forms ((clause always-clause))
  `((unless ,(test-form clause)
      (setq ,(temp-var clause) nil)
      (hoop-finish))))

(defmethod next-movable-forms ((clause always-clause))
  `((unless ,(test-form clause)
      (setq ,(temp-var clause) nil)
      (hoop-finish))))

(defclass never-clause (termination-with-return-clause)
  ())

(defmethod make-clause ((type (eql :never)) &rest initargs)
  (apply #'make-instance 'never-clause :test initargs))

(defmethod wrap-outer-form ((clause never-clause) form)
  `(let ((,(temp-var clause) t))
     ,form))

(defmethod initial-movable-forms ((clause never-clause))
  `((when ,(test-form clause)
      (setq ,(temp-var clause) nil)
      (hoop-finish))))

(defmethod next-movable-forms ((clause never-clause))
  `((when ,(test-form clause)
      (setq ,(temp-var clause) nil)
      (hoop-finish))))

(defclass thereis-clause (termination-with-return-clause)
  ())

(defmethod make-clause ((type (eql :thereis)) &rest initargs)
  (apply #'make-instance 'thereis-clause :test initargs))

(defmethod wrap-outer-form ((clause thereis-clause) form)
  `(let ((temp-var clause))
     ,form))

(defmethod initial-movable-forms ((clause thereis-clause))
  `((setq ,(temp-var clause) ,(test-form clause))
    (when ,(temp-var clause)
      (hoop-finish))))

(defmethod next-movable-forms ((clause thereis-clause))
  `((setq ,(temp-var clause) ,(test-form clause))
    (when ,(temp-var clause)
      (hoop-finish))))

(defclass repeat-clause ()
  ((remaining-var :reader remaining-var
                  :initform (gensym "REMAINING"))
   (count-form :reader count-form
               :initarg :count)))

(defmethod make-clause ((type (eql :repeat)) &rest initargs)
  (apply #'make-instance 'repeat-clause :count initargs))

(defmethod wrap-outer-form ((clause repeat-clause) form)
  `(let ((,(remaining-var clause) ,(count-form clause)))
     ,form))

(defmethod initial-early-forms ((clause repeat-clause))
  `((unless (plusp ,(remaining-var clause))
      (hoop-finish))))

(defmethod next-early-forms ((clause repeat-clause))
  `((unless (plusp (decf ,(remaining-var clause)))
     (hoop-finish))))
