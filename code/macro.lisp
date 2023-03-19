(in-package #:hoop)

(defmacro hoop-finish ())

(defmacro hoop (clauses &body body)
  (multiple-value-bind (forms declarations)
      (uiop:parse-body body)
    (let ((clauses (mapcar (lambda (args)
                             (apply #'make-clause args))
                           clauses))
          (repeat-tag (gensym))
          (finish-tag (gensym)))
      `(block nil
         ,(reduce #'wrap-outer
                  clauses :from-end t
                  :initial-value `(let* ,(mapcan #'bindings clauses)
                                    ,.declarations
                                    ,.(mapcan #'declarations clauses)
                                    (macrolet ((hoop-finish ()
                                                 (list 'go ',finish-tag)))                                                              
                                      ,(reduce #'wrap-inner                                     
                                               clauses
                                               :from-end t
                                               :initial-value `(tagbody
                                                                ,repeat-tag
                                                                 ,.(mapcan #'prologue-forms clauses)
                                                                 ,.forms
                                                                 ,.(mapcan #'epilogue-forms clauses)
                                                                 (go ,repeat-tag)
                                                                ,finish-tag
                                                                 ,.(mapcan #'finish-forms clauses)
                                                                 (return ,(block nil
                                                                            (mapc (lambda (clause)
                                                                                    (multiple-value-bind (returnp form)
                                                                                        (return-form clause)
                                                                                      (when returnp
                                                                                        (return form))))
                                                                                  clauses))))))))))))
