(in-package #:hoop)

(defmacro hoop-finish ())

(defun find-first (function clauses)
  (dolist (clause clauses)
    (multiple-value-bind (returnp form)
        (funcall function clause)
      (when returnp
        (return-from find-first form)))))

(defmacro hoop (clauses &body body)
  (let* ((clauses (mapcar (lambda (args)
                            (apply #'make-clause args))
                          clauses))
         (repeat-tag (gensym))
         (finish-tag (gensym))
         (body-form `(macrolet ((hoop-finish ()
                                  (list 'go ',finish-tag)))
                       (tagbody
                        ,repeat-tag
                         ,.(mapcan #'prologue-forms clauses)
                         ,@body
                         ,.(mapcan #'epilogue-forms clauses)
                         (go ,repeat-tag)
                        ,finish-tag
                         ,.(mapcan #'finish-forms clauses)
                         (return ,(find-first #'return-form clauses))))))
    `(block ,(find-first #'block-name clauses)
       ,(reduce #'wrap-form clauses
                :from-end t :initial-value body-form))))
