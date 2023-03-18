(in-package #:hoop)

(defclass list-clause (clause)
  ((list-var :reader list-var
             :initform (gensym))
   (by :reader by
       :initarg :by)))

(defclass in-clause (list-clause)
  ())

(defclass on-clause (list-clause)
  ())

(defmethod expand (var (action (eql :in-list)) &optional initform &rest initargs &key by &allow-other-keys)
  (declare (ignore initargs))
  (make-instance 'in-clause :var var :initform initform :by (or by #'cdr)))

(defmethod expand (var (action (eql :on-list)) &optional initform &rest initargs &key by &allow-other-keys)
  (declare (ignore initargs))
  (make-instance 'on-clause :var var :initform initform :by (or by #'cdr)))

(defmethod bindings ((clause list-clause))
  `((,(list-var clause) ,(initform clause))))

(defmethod wrap-inner ((clause in-clause) form)
  `(symbol-macrolet ,(symbol-macros-from-d-var-spec (var clause) `(car ,(list-var clause)))
     ,form))

(defmethod wrap-inner ((clause on-clause) form)
  `(symbol-macrolet ,(symbol-macros-from-d-var-spec (var clause) (list-var clause))
     ,form))

(defmethod prologue ((clause list-clause))
  `((unless ,(list-var clause) (hoop-finish))))

(defmethod epilogue ((clause list-clause))
  `((setf ,(list-var clause) (funcall ,(by clause) ,(list-var clause)))))

