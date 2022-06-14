(in-package #:hoop)

(defmacro hoop-finish ())

(defstruct hoop-expansion
  bindings
  functions
  symbol-macros
  macros
  declarations
  prologue
  epilogue)

(defgeneric hoop-expand (var action initform &rest initargs &key &allow-other-keys))

(defmethod hoop-expand (var (action (eql :=)) initform &rest initargs
                        &key (next nil nextp) (test nil testp) &allow-other-keys)
  (declare (ignore initargs))
  (make-hoop-expansion :bindings (list (list var initform))
                       :prologue (when testp
                                   (list `(when ,test (hoop-finish))))
                       :epilogue (when nextp
                                   (list `(setf ,var ,next)))))

(defmethod hoop-expand (var (action (eql :up)) initform &rest initargs
                        &key (to nil top) (end nil endp) (by nil byp) &allow-other-keys)
  (declare (ignore initargs))
  (make-hoop-expansion :bindings (list (list var initform))
                       :prologue (cond (top
                                        (list `(when (> ,var ,to) (hoop-finish))))
                                       (endp
                                        (list `(unless (< ,var ,end) (hoop-finish)))))
                       :epilogue (list `(incf ,var ,(if byp by 1)))))

(defmethod hoop-expand (var (action (eql :down)) initform &rest initargs
                        &key (to nil top) (end nil endp) (by nil byp) &allow-other-keys)
  (declare (ignore initargs))
  (make-hoop-expansion :bindings (list (list var initform))
                       :prologue (cond (top
                                        (list `(when (< ,var ,to) (hoop-finish))))
                                       (endp
                                        (list `(unless (> ,var ,end) (hoop-finish)))))
                       :epilogue (list `(decf ,var ,(if byp by 1)))))

(defmethod hoop-expand (var (action (eql :in)) initform &rest initargs &key by &allow-other-keys)
  (declare (ignore initargs))
  (let ((list-var (gensym)))
    (make-hoop-expansion :bindings (list (list list-var initform))
                         :symbol-macros (list (list var `(car ,list-var)))
                         :prologue (list `(unless ,list-var (hoop-finish)))
                         :epilogue (list `(setf ,list-var (funcall ,(or by #'cdr) ,list-var))))))

(defmethod hoop-expand (var (action (eql :key)) initform &rest initargs &key (value nil valuep) &allow-other-keys)
  (declare (ignore initargs))
  (let ((iterator-var (gensym))
        (next-var (gensym)))
    (make-hoop-expansion :bindings (list* var next-var
                                          (list iterator-var `(with-hash-table-iterator (f ,initform) (lambda () (f))))
                                          (when valuep
                                            (list value)))
                         :prologue (list `(multiple-value-setq ,(if valuep
                                                                    (list next-var var value)
                                                                    (list next-var var))
                                              (funcall ,iterator-var))
                                         `(unless ,next-var (hoop-finish))))))

(defmethod hoop-expand (var (action (eql :alist)) initform &rest initargs &key (value nil valuep) &allow-other-keys)
  (declare (ignore initargs))
  (let ((list-var (gensym)))
    (make-hoop-expansion :bindings (list (list list-var initform))
                         :symbol-macros (list* (list var `(caar ,list-var))
                                               (when valuep
                                                 (list (list value `(cdar ,list-var)))))
                         :prologue (list `(unless ,list-var (hoop-finish)))
                         :epilogue (list `(pop ,list-var)))))

(defmethod hoop-expand (var (action (eql :plist)) initform &rest initargs &key (value nil valuep) &allow-other-keys)
  (declare (ignore initargs))
  (let ((list-var (gensym)))
    (make-hoop-expansion :bindings (list (list list-var initform))
                         :symbol-macros (list* (list var `(car ,list-var))
                                               (when valuep
                                                 (list (list value `(cadr ,list-var)))))
                         :prologue (list `(unless ,list-var (hoop-finish)))
                         :epilogue (list `(setf ,list-var (cddr ,list-var))))))

(defmethod hoop-expand (var (action (eql :across)) initform &rest initargs
                        &key key start end from-end &allow-other-keys)
  (declare (ignore initargs))
  (let ((seq-var (gensym))
        (index-var (gensym)))
    (if from-end
        (make-hoop-expansion :bindings (list (list seq-var initform)
                                             (list index-var (or start (1- (length seq-var)))))
                             :symbol-macros (list (list var `(aref ,seq-var ,index-var)))
                             :prologue (list `(unless (> ,index-var (or ,end -1) ,seq-var)
                                                (hoop-finish)))
                             :epilogue (list `(decf ,index-var)))
        (make-hoop-expansion :bindings (list (list seq-var initform)
                                             (list index-var (or start 0)))
                             :symbol-macros (list (list var `(aref ,seq-var ,index-var)))
                             :prologue (list `(unless (< ,index-var (or ,end (length ,seq-var)))
                                                (hoop-finish)))
                             :epilogue (list `(incf ,index-var))))))

(defmethod hoop-expand (var (action (eql :collect)) initform &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let ((tail-var (gensym)))
    (make-hoop-expansion :bindings (list (list var initform)
                                         (list tail-var `(last ,var)))
                         :functions (list `(,var (x)
                                             (if ,tail-var
                                                 (setf (cdr ,tail-var) (list x)
                                                       ,tail-var (cdr ,tail-var))
                                                 (setf ,var (list x)
                                                       ,tail-var ,var))
                                             x)))))

(defmacro hoop (bindings return-form &body body)
  (let ((expansions (mapcar (lambda (args) (apply #'hoop-expand args)) bindings))
        (repeat-tag (gensym))
        (finish-tag (gensym)))
    (multiple-value-bind (remaining-forms declarations)
        (uiop:parse-body body)
      `(block nil
         (let* ,(mapcan #'hoop-expansion-bindings expansions)
           (flet ,(mapcan #'hoop-expansion-functions expansions)
             (macrolet ((hoop-finish ()
                          (list 'go ',finish-tag))
                        ,@(mapcan #'hoop-expansion-macros expansions))
               (symbol-macrolet ,(mapcan #'hoop-expansion-symbol-macros expansions)
                 (tagbody
                  ,repeat-tag
                   ,@declarations
                   ,@(mapcan #'hoop-expansion-declarations expansions)
                   ,@(mapcan #'hoop-expansion-prologue expansions)
                   ,@remaining-forms
                   ,@(mapcan #'hoop-expansion-epilogue expansions)
                   (go ,repeat-tag)
                  ,finish-tag
                   (return ,return-form))))))))))