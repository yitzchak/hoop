(in-package #:hoop)

(defmacro hoop-finish ())

(defmacro hoop (clauses &body body)
  (multiple-value-bind (forms declarations)
      (uiop:parse-body body)
    (let* ((expansions (mapcar (lambda (args)
                                 (apply #'make-clause args))
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
                                                                  ,.(mapcan #'prologue-forms expansions)
                                                                  ,.forms
                                                                  ,.(mapcan #'epilogue-forms expansions)
                                                                  (go ,repeat-tag)
                                                                ,finish-tag
                                                                  (return ,(some #'return-form expansions)))))))))))
