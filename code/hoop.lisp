(in-package #:hoop)

(defmacro hoop-finish ())

(defgeneric expand (var action &optional initform &rest initargs &key &allow-other-keys))

(defgeneric wrap-inner (clause form)
  (:method (clause form)
    (declare (ignore clause))
    form))

(defgeneric wrap-outer (clause form)
  (:method (clause form)
    (declare (ignore clause))
    form))

(defgeneric bindings (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric declarations (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric prologue (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric epilogue (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defclass clause ()
  ((var :reader var
        :initarg :var)
   (initform :reader initform
             :initarg :initform)))

(defun symbol-macros-from-d-var-spec (var-spec form)
  (check-type var-spec (or symbol cons))
  (cond ((null var-spec)
         nil)
        ((symbolp var-spec)
         `((,var-spec ,form)))
        (t
         (nconc (symbol-macros-from-d-var-spec (car var-spec) `(car ,form))
                (symbol-macros-from-d-var-spec (cdr var-spec) `(cdr ,form))))))

(defmacro hoop (clauses return-form &body body)
  (multiple-value-bind (forms declarations)
      (uiop:parse-body body)
    (let* ((expansions (mapcar (lambda (args)
                                 (apply #'expand args))
                               clauses))
           (repeat-tag (gensym))
           (finish-tag (gensym)))
      `(block nil
         ,(reduce #'wrap-outer
                  expansions :from-end t
                  :initial-value `(let* ,(mapcan #'bindings expansions)
                                    ,.declarations
                                    ,.(mapcan #'declarations expansions)
                                    (macrolet ((hoop-finish ()
                                                 (list 'go ',finish-tag)))                                                              
                                      ,(reduce #'wrap-inner                                     
                                               expansions
                                               :from-end t
                                               :initial-value `(tagbody
                                                                ,repeat-tag
                                                                  ,.(mapcan #'prologue expansions)
                                                                  ,.forms
                                                                  ,.(mapcan #'epilogue expansions)
                                                                  (go ,repeat-tag)
                                                                ,finish-tag
                                                                  (return ,return-form))))))))))
