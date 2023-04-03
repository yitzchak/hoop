(in-package #:hoop)

(defparameter *declaration-specifiers* nil)

(defun parse-body (body)
  (prog (primary-specifiers secondary-specifiers)
   next
     (when (and (not (endp body))
                (eq (caar body) 'declare))
       (dolist (specifier (cdar body))
         (if (member (car specifier)
                     '(dynamic-extent inline notinline type ftype ignore ignorable special))
             (push specifier primary-specifiers)
             (push specifier secondary-specifiers)))
       (setf body (cdr body))
       (go next))
     (return (values primary-specifiers secondary-specifiers body))))

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
