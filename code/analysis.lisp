(in-package #:hoop)

(defun analyze (clauses)
  (loop for (variable . rest) on (mapcan #'variable-names clauses)
        when (member variable rest)
          do (error 'program-error)))
