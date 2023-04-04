(in-package #:hoop/test)


(defparameter *hoop.6.alist*
  '((a . 1) (b . 2) (c . 3)))

(defparameter *hoop.6.alist.2*
  '(("a" . 1) ("b" . 2) ("c" . 3)))

(defparameter *hoop.6.alist.3*
  '(((a1 . a2) . 1) ((b1 . b2) . 2) ((c1 . c2) . 3)))

(defparameter *hoop.6.hash.1*
  (let ((table (make-hash-table :test #'eq)))
    (dolist (pair *hoop.6.alist*)
      (setf (gethash (car pair) table) (cdr pair)))
    table))

(defparameter *hoop.6.hash.2*
  (let ((table (make-hash-table :test #'eql)))
    (dolist (pair *hoop.6.alist*)
      (setf (gethash (car pair) table) (cdr pair)))
    table))

(defparameter *hoop.6.hash.3*
  (let ((table (make-hash-table :test #'equal)))
    (dolist (pair *hoop.6.alist.3*)
      (setf (gethash (car pair) table) (cdr pair)))
    table))

;;; (defparameter *hoop.6.hash.4*
;;;  (let ((table (make-hash-table :test #'equalp)))
;;;    (hoop* for (key . val) in *hoop.6.alist.2*
;;;       do (setf (gethash key table) val))
;;;    table))

(defparameter *hoop.6.hash.5*
  (let ((table (make-hash-table :test #'eql)))
    (dolist (pair *hoop.6.alist.3*)
      (setf (gethash (cdr pair) table) (car pair)))
    table))

(defparameter *hoop.6.hash.6*
  (let ((table (make-hash-table :test #'eq)))
    (dolist (pair *hoop.6.alist*)
      (setf (gethash (car pair) table) (cdr pair)))
    table))

(defparameter *hoop.6.hash.7*
  (let ((table (make-hash-table :test #'equal)))
    (dolist (pair *hoop.6.alist.3*)
      (setf (gethash (car pair) table) (cdr pair)))
    table))

(defparameter *hoop.6.alist.8*
  '(((1 . 2) . 1) ((3 . 4) . b) ((5 . 6) . c)))

(defparameter *hoop.6.hash.8*
  (let ((table (make-hash-table :test #'equal)))
    (dolist (pair *hoop.6.alist.8*)
      (setf (gethash (car pair) table) (cdr pair)))
    table))

(defparameter *hoop.6.hash.9*
  (let ((table (make-hash-table :test #'equal)))
    (dolist (pair *hoop.6.alist.8*)
      (setf (gethash (cdr pair) table) (car pair)))
    table))

(define-test hoop.6.1
  :compile-at :execute
  (is equal
      6
      (hoop* ((:each-key-value (nil x) :in *hoop.6.hash.1*)
              (:sum c))
        (c x))))

(define-test hoop.6.6
  :compile-at :execute
  (is equal
      '(a b c)
      (sort (hoop* ((:each-key-value (x nil) :in *hoop.6.hash.1*)
                    (:collect c))
              (c x))
            #'symbol<)))

(define-test hoop.6.11
  :compile-at :execute
  (is equal
      '(a1 b1 c1)
      (sort (hoop* ((:each-key-value ((u . v) nil) :in *hoop.6.hash.3*)
                    (:collect c))
              (c u))
            #'symbol<)))

(define-test hoop.6.12
  :compile-at :execute
  (is equal
      '(a2 b2 c2)
      (sort (hoop* ((:each-key-value ((u . v) nil) :in *hoop.6.hash.3*)
                    (:collect c))
              (c v))
            #'symbol<)))

(define-test hoop.6.13
  :compile-at :execute
  (is equal
      '(a1 b1 c1)
      (sort (hoop* ((:each-key-value (nil (u . v)) :in *hoop.6.hash.5*)
                    (:collect c))
              (c u))
            #'symbol<)))

(define-test hoop.6.14
  :compile-at :execute
  (is equal
      '(a2 b2 c2)
      (sort (hoop* ((:each-key-value (nil (u . v)) :in *hoop.6.hash.5*)
                    (:collect c))
              (c v))
            #'symbol<)))

(define-test hoop.6.15
  :compile-at :execute
  (is equal
      '((a 1) (b 2) (c 3))
      (sort (hoop* ((:each-key-value (k v) :in *hoop.6.hash.1*)
                    (:collect c))
              (c (list k v)))
            #'< :key #'second)))

(define-test hoop.6.17
  :compile-at :execute
  (is equal
      '(a1 b1 c1)      
      (sort (hoop* ((:each-key-value (nil (u . nil)) :in *hoop.6.hash.5*)
                    (:collect c))
              (c u))
            #'symbol<)))

(define-test hoop.6.18
  :compile-at :execute
  (is equal
      '(a2 b2 c2)      
      (sort (hoop* ((:each-key-value (nil (nil . v)) :in *hoop.6.hash.5*)
                    (:collect c))
              (c v))
            #'symbol<)))

(define-test hoop.6.19
  :compile-at :execute
  (is equal
      3
      (hoop* ((:each-key-value (nil nil) :in *hoop.6.hash.5*)
              (:count c))
        (c t))))

(define-test hoop.6.23
  :compile-at :execute
  (is equal
      6
      (hoop* ((:each-key-value (nil v) :in *hoop.6.hash.1*)
              (:sum c))
        (declare (type fixnum c)
                 (type (or null fixnum) v))
        (c v))))

(define-test hoop.6.25
  :compile-at :execute
  (is equal
      6
      (hoop* ((:each-key-value (k nil) :in *hoop.6.hash.5*)
              (:sum c))
        (c k))))

(define-test hoop.6.35
  :compile-at :execute
  (is equal
      21
      (hoop* ((:each-key-value ((k1 . k2) nil) :in *hoop.6.hash.8*)
              (:sum c))
        (c k1 k2))))

(define-test hoop.6.36
  :compile-at :execute
  (is equal
      21
      (hoop* ((:each-key-value (nil (v1 . v2)) :in *hoop.6.hash.9*)
              (:sum c))
        (c v1 v2))))

#|;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(define-test hoop.6.41
(macrolet
((%m (z) z))
(hoop* ((:each-key-value x being the hash-value :in
(expand-in-current-env (%m *hoop.6.hash.1*)) sum x))
6)

(define-test hoop.6.42
(macrolet
((%m (z) z))
(sort (hoop* ((:each-key-value x being the hash-key :in
(expand-in-current-env (%m *hoop.6.hash.1*)) collect x)
#'symbol<))
(a b c))

;;; Error tests

(define-test hoop.6.error.1
(signals-error
(hoop* ((:each-key-value k from 1 to 10
((:each-key-value k being the hash-keys :in *hoop.6.hash.1*
count t)
program-error)
t)

(define-test hoop.6.error.2
(signals-error
(hoop* ((:each-key-value k being the hash-keys :in *hoop.6.hash.1*
((:each-key-value k from 1 to 10
count t)
program-error)
t)

(define-test hoop.6.error.3
(signals-error
(hoop* ((:each-key-value (k . k) being the hash-keys :in *hoop.6.hash.3*
count t)
program-error)
t)

(define-test hoop.6.error.4
(signals-error
(hoop* ((:each-key-value k being the hash-keys :in *hoop.6.hash.3*
using (hash-value k)
count t)
program-error)
t)

(define-test hoop.6.error.5
(signals-error
(hoop* ((:each-key-value k being the hash-values :in *hoop.6.hash.3*
using (hash-key k)
count t)
program-error)
t)
|#
