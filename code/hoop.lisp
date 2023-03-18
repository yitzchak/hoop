(in-package #:hoop)

(defmacro hoop-finish ())

#|(defgeneric hoop-expand (var action initform &rest initargs &key &allow-other-keys))

(defmethod hoop-expand (var (action (eql :=)) initform &rest initargs
                        &key (next nil nextp) (test nil testp) &allow-other-keys)
  (declare (ignore initargs))
  (list :bindings (list (list var initform))
        :prologue (when testp
                    (list `(when ,test (hoop-finish))))
        :epilogue (when nextp
                    (list `(setf ,var ,next)))))

(defmethod hoop-expand (var (action (eql :up)) initform &rest initargs
                        &key (to nil top) (end nil endp) (by nil byp) &allow-other-keys)
  (declare (ignore initargs))
  (list :bindings (list (list var initform))
        :prologue (cond (top
                         (list `(when (> ,var ,to) (hoop-finish))))
                        (endp
                         (list `(unless (< ,var ,end) (hoop-finish)))))
        :epilogue (list `(incf ,var ,(if byp by 1)))))

(defmethod hoop-expand (var (action (eql :down)) initform &rest initargs
                        &key (to nil top) (end nil endp) (by nil byp) &allow-other-keys)
  (declare (ignore initargs))
  (list :bindings (list (list var initform))
        :prologue (cond (top
                         (list `(when (< ,var ,to) (hoop-finish))))
                        (endp
                         (list `(unless (> ,var ,end) (hoop-finish)))))
        :epilogue (list `(decf ,var ,(if byp by 1)))))

(defmethod hoop-expand (var (action (eql :across)) initform &rest initargs
                        &key key start end from-end &allow-other-keys)
  (declare (ignore initargs))
  (let ((seq-var (gensym))
        (index-var (gensym)))
    (if from-end
        (list :bindings (list (list seq-var initform)
                              (list index-var (or start (1- (length seq-var)))))
              :symbol-macros (list (list var `(aref ,seq-var ,index-var)))
              :prologue (list `(unless (> ,index-var (or ,end -1) ,seq-var)
                                 (hoop-finish)))
              :epilogue (list `(decf ,index-var)))
        (list :bindings (list (list seq-var initform)
                              (list index-var (or start 0)))
              :symbol-macros (list (list var `(aref ,seq-var ,index-var)))
              :prologue (list `(unless (< ,index-var (or ,end (length ,seq-var)))
                                 (hoop-finish)))
              :epilogue (list `(incf ,index-var))))))|#

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
