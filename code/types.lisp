(in-package #:hoop)

(defun d-var-spec-p (object)
  (or (symbolp object)
      (and (consp object)
           (d-var-spec-p (car object))
           (d-var-spec-p (cdr object)))))

(deftype d-var-spec ()
  `(satisfies d-var-spec-p))
