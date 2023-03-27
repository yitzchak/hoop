(in-package #:hoop)

(defclass termination-clause ()
  ((test-form :reader test-form
              :initarg :test)))

(defclass while-clause (termination-clause)
  ())

(defmethod make-clause ((type (eql :while)) &rest initargs)
  (apply #'make-instance 'while-clause :test initargs))

(defmethod before-forms ((clause while-clause))
  `((unless ,(test-form clause)
      (hoop-finish))))

(defclass until-clause (termination-clause)
  ((test-form :reader test-form
              :initarg :test)))

(defmethod make-clause ((type (eql :until)) &rest initargs)
  (apply #'make-instance 'until-clause :test initargs))

(defmethod before-forms ((clause until-clause))
  `((when ,(test-form clause)
      (hoop-finish))))

(defclass always-clause (termination-clause)
  ())

(defmethod make-clause ((type (eql :always)) &rest initargs)
  (apply #'make-instance 'always-clause :test initargs))

(defmethod before-forms ((clause always-clause))
  `((unless ,(test-form clause)
      (return nil))))

(defmethod return-form ((clause always-clause))
  (values t t))

(defclass never-clause (termination-clause)
  ())

(defmethod make-clause ((type (eql :never)) &rest initargs)
  (apply #'make-instance 'never-clause :test initargs))

(defmethod before-forms ((clause never-clause))
  `((when ,(test-form clause)
      (return nil))))

(defmethod return-form ((clause never-clause))
  (values t t))

(defclass thereis-clause (termination-clause temp-var-slot)
  ())

(defmethod make-clause ((type (eql :thereis)) &rest initargs)
  (apply #'make-instance 'thereis-clause :test initargs))

(defmethod before-forms ((clause thereis-clause))
  `((let ((,(temp-var clause) ,(test-form clause)))
      (when ,(temp-var clause)
        (return ,(temp-var clause))))))

(defmethod return-form ((clause thereis-clause))
  (values t nil))

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

(defmethod before-forms ((clause repeat-clause))
  `((unless (plusp ,(remaining-var clause))
      (hoop-finish))))

(defmethod after-forms ((clause repeat-clause))
  `((decf ,(remaining-var clause))))
