(in-package #:hoop)

(defparameter *declaration-specifiers* nil)

(defparameter *standard-typespec-identifiers*
  '(and array
    base-string bit-vector
    complex cons
    double-float
    eql
    float function
    integer
    long-float
    member mod
    not
    or
    rational real
    satisfies  short-float signed-byte
    simple-array simple-base-string simple-bit-vector simple-string
    single-float simple-vector string
    unsigned-byte
    values
    vector))

(defparameter *standard-declaration-identitiers*
  '(inline notinline dynamic-extent type ftype ignore ignorable special optimize))

(defun type-specifier-p (x &optional env)
  (or (consp x)
      (typep x 'class)
      (and (symbolp x)
           (not (member x *standard-declaration-identitiers*))
           (or (member x *standard-typespec-identifiers*)
               #+sbcl (sb-int:info :type :kind x)
               #+ccl (ccl::specifier-type x env)
               (documentation x 'type)
               (ignore-errors (nth-value 1 (subtypep x nil env)))))))

(defun parse-body (body env)
  (prog (specifiers)
   next
     (when (and (not (endp body))
                (eq (caar body) 'declare))
       (dolist (specifier (cdar body))
         (push (if (type-specifier-p (car specifier) env)
                   (cons 'type specifier)
                   specifier)
               specifiers))
       (setf body (cdr body))
       (go next))
     (return (values specifiers body))))

(defun copy-specifier (head tail targets &optional functionp)
  (let ((new-tail (remove-if (lambda (target)
                               (not (member (if functionp
                                                `(function ,target)
                                                target)
                                            targets :test #'equal)))
                             tail)))
    (when new-tail
      (list (nconc head new-tail)))))

(defun declarations (targets)
  (let ((specifiers (mapcan (lambda (specifier)
                              (case (car specifier)
                                ((dynamic-extent ignorable ignore special)
                                 (copy-specifier (list (car specifier))
                                                 (cdr specifier)
                                                 targets))
                                (type
                                 (copy-specifier (list (car specifier) (cadr specifier))
                                                 (cddr specifier)
                                                 targets))
                                ((inline notinline)
                                 (copy-specifier (list (car specifier))
                                                 (cdr specifier)
                                                 targets
                                                 t))
                                (ftype
                                 (copy-specifier (list (car specifier) (cadr specifier))
                                                 (cddr specifier)
                                                 targets
                                                 t))))
                            *declaration-specifiers*)))
    (when specifiers
      `((declare ,.specifiers)))))

(defun copy-remaining-specifier (head tail targets &optional functionp)
  (let ((new-tail (remove-if (lambda (target)
                               (member (if functionp
                                                `(function ,target)
                                                target)
                                            targets :test #'equal))
                             tail)))
    (when new-tail
      (list (nconc head new-tail)))))

(defun remaining-declarations (targets)
  (let ((specifiers (mapcan (lambda (specifier)
                              (case (car specifier)
                                ((dynamic-extent ignorable ignore special)
                                 (copy-remaining-specifier (list (car specifier))
                                                           (cdr specifier)
                                                           targets))
                                (type
                                 (copy-remaining-specifier (list (car specifier) (cadr specifier))
                                                           (cddr specifier)
                                                           targets))
                                ((inline notinline)
                                 (copy-remaining-specifier (list (car specifier))
                                                           (cdr specifier)
                                                           targets
                                                           t))
                                (ftype
                                 (copy-remaining-specifier (list (car specifier) (cadr specifier))
                                                           (cddr specifier)
                                                           targets
                                                           t))
                                (otherwise
                                 (list specifier))))
                            *declaration-specifiers*)))
    (when specifiers
      `((declare ,.specifiers)))))


(defun get-type (target &optional (default t))
  (let ((specifier (find-if (lambda (specifier)
                              (or (and (symbolp target)
                                       (eq (car specifier) 'type)
                                       (member target (cddr specifier)))
                                  (and (consp target)
                                       (eq (car target) 'function)
                                       (eq (car specifier) 'ftype)
                                       (member (cadr target) (cddr specifier)))))
                            *declaration-specifiers*)))
    (if specifier
        (cadr specifier)
        default)))

(defun type-declarations (target &rest targets)
  (let ((specifier (find-if (lambda (specifier)
                              (or (and (symbolp target)
                                       (eq (car specifier) 'type)
                                       (member target (cddr specifier)))
                                  (and (consp target)
                                       (eq (car target) 'function)
                                       (eq (car specifier) 'ftype)
                                       (member (cadr target) (cddr specifier)))))
                            *declaration-specifiers*)))
    (when specifier
      `((declare (,(first specifier)
                  ,(second specifier)
                  ,@targets))))))
