(cl:in-package #:hoop/test)

(defun symbol< (x &rest args)
  (apply #'string< (symbol-name x) (mapcar #'symbol-name args)))

(defmacro expand-in-current-env (macro-form &environment env)
  (macroexpand macro-form env))
