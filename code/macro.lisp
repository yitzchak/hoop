(in-package #:hoop)

(defmacro hoop-next ())

(defmacro hoop-finish ())

(defun find-first (function clauses)
  (dolist (clause clauses)
    (multiple-value-bind (returnp form)
        (funcall function clause)
      (when returnp
        (return-from find-first form)))))

(defun expand (type clauses body)
  (let* ((clause (apply #'make-clause type clauses))
        (before-tag (gensym))
        (after-tag (gensym))
        (epilogue-tag (gensym))
        (block-name (nth-value 2 (block-name clause))))
    (analyze clause)
    `(block ,block-name
       ,(wrap-outer-form clause
                         (wrap-inner-form clause
                                          `(macrolet ((hoop-next ()
                                                        (list 'go ',after-tag))
                                                      (hoop-finish ()
                                                        (list 'go ',epilogue-tag)))
                                             (tagbody
                                                ,.(prologue-forms clause)
                                                ,.(initial-early-forms clause)
                                                ,.(initial-late-forms clause)
                                              ,before-tag
                                                ,.(before-forms clause)
                                                ,@body
                                              ,after-tag  
                                                ,.(after-forms clause)
                                                ,.(next-early-forms clause)
                                                ,.(next-late-forms clause)
                                                (go ,before-tag)
                                              ,epilogue-tag
                                                ,.(epilogue-forms clause)
                                                (return-from ,block-name
                                                  ,(multiple-value-bind (returnp form)
                                                       (return-form clause)
                                                     (if returnp
                                                         form
                                                         `(values ,.(return-value-forms clause))))))))))))

(defmacro hoop (clauses &body body)
  (expand :parallel clauses body))

(defmacro hoop* (clauses &body body)
  (expand :serial clauses body))
