(in-package #:hoop)

(defmacro hoop-finish ())

(defmacro hoop (clauses &body body)
  (multiple-value-bind (forms declarations)
      (uiop:parse-body body)
    (let* ((clauses (mapcar (lambda (args)
                              (apply #'make-clause args))
                            clauses))
           (repeat-tag (gensym))
           (finish-tag (gensym))
           (return-form (block nil
                          (mapc (lambda (clause)
                                  (multiple-value-bind (returnp form)
                                      (return-form clause)
                                    (when returnp
                                      (return form))))
                                clauses)
                          nil))
           (body-form `(macrolet ((hoop-finish ()
                                    (list 'go ',finish-tag)))
                         (tagbody
                            ,repeat-tag
                            ,.(mapcan #'prologue-forms clauses)
                            ,.forms
                            ,.(mapcan #'epilogue-forms clauses)
                            (go ,repeat-tag)
                            ,finish-tag
                            ,.(mapcan #'finish-forms clauses)
                            (return ,return-form)))))
      `(block nil
         (let* ,(mapcan #'bindings clauses)
           ,.declarations
           ,.(mapcan #'declarations clauses)
           ,(reduce #'wrap-form clauses
                    :from-end t :initial-value body-form))))))
