(in-package #:hoop)

(defun analyze (clause)
  (loop for (variable . rest) on (variable-names clause)
        when (member variable rest)
          do (error 'program-error)))
