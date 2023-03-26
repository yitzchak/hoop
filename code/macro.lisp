(in-package #:hoop)

(defmacro hoop-next ())

(defmacro hoop-finish ())

(defun find-first (function clauses)
  (dolist (clause clauses)
    (multiple-value-bind (returnp form)
        (funcall function clause)
      (when returnp
        (return-from find-first form)))))

(defmacro hoop (clauses &body body)
  (let ((clauses (mapcar (lambda (args)
                           (apply #'make-clause nil args))
                         clauses)))
    (analyze clauses)
    (let* ((before-tag (gensym))
           (after-tag (gensym))
           (epilogue-tag (gensym))
           (body-form `(macrolet ((hoop-next ()
                                    (list 'go ',after-tag))
                                  (hoop-finish ()
                                    (list 'go ',epilogue-tag)))
                         (tagbody
                            ,.(mapcan #'prologue-forms clauses)
                          ,before-tag
                            ,.(mapcan #'termination-forms clauses)
                            ,.(mapcan #'before-forms clauses)
                            ,@body
                          ,after-tag  
                            ,.(mapcan #'after-forms clauses)
                            (go ,before-tag)
                          ,epilogue-tag
                            ,.(mapcan #'epilogue-forms clauses)
                            (return ,(find-first #'return-form clauses))))))
      `(block ,(find-first #'block-name clauses)
         ,(reduce #'wrap-outer-form clauses
                  :from-end t
                  :initial-value (reduce #'wrap-inner-form clauses
                                         :from-end t :initial-value body-form))))))
