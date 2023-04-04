(in-package #:hoop)

(defparameter *declaration-specifiers* nil)

(defun parse-body (body)
  (prog (specifiers)
   next
     (when (and (not (endp body))
                (eq (caar body) 'declare))
       (setf specifiers (append specifiers (cdar body))
             body (cdr body))
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

(defun declarations (&rest targets)
  (let ((specifiers (mapcan (lambda (specifier)
                              (case (car specifier)
                                ((dynamic-extent inline notinline ignorable ignore special)
                                 (copy-specifier (list (car specifier))
                                                 (cdr specifier)
                                                 targets))
                                (type
                                 (copy-specifier (list (car specifier) (cadr specifier))
                                                 (cddr specifier)
                                                 targets))
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
                                ((dynamic-extent inline notinline ignorable ignore special)
                                 (copy-remaining-specifier (list (car specifier))
                                                           (cdr specifier)
                                                           targets))
                                (type
                                 (copy-remaining-specifier (list (car specifier) (cadr specifier))
                                                           (cddr specifier)
                                                           targets))
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
