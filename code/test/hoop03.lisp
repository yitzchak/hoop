(in-package #:hoop/test)

(define-test hoop.3.1
  :compile-at :execute
  (is equal
      6
      (hoop ((:each-sublist x :in '(1 2 3))
             (:sum c))
        (c (car x)))))

(define-test hoop.3.2
  :compile-at :execute
  (is equal
      '(2 3 4)
      (hoop ((:each-sublist x :in '(1 2 3 4)))
        (when (evenp (car x)) (return x)))))

#|(define-test hoop.3.3
  :compile-at :execute
  (is equal
  (hoop ((:each-sublist x :in '(a b c . d) collect (car x))
  (a b c))

(define-test hoop.3.4
  :compile-at :execute
  (is equal
  (let ((x nil))
    (hoop ((:each-sublist e :in '(a b c d) do (push (car e) x))
    x)
  (d c b a))

(define-test hoop.3.5
  :compile-at :execute
  (is equal
  (hoop ((:each-sublist e :in '(a b c d e f) :by #'cddr
        collect (car e))
  (a c e))

(define-test hoop.3.6
  :compile-at :execute
  (is equal
  (hoop ((:each-sublist e :in '(a b c d e f g) :by #'cddr
        collect (car e))
  (a c e g))

(define-test hoop.3.7
  :compile-at :execute
  (is equal
  (hoop ((:each-sublist e :in '(a b c d e f)
        :by #'(lambda (l) (and (cdr l) (cons (car l) (cddr l))))
        collect (car e))
  (a a a a a a))

(define-test hoop.3.8
  :compile-at :execute
  (is equal
  (hoop ((:each-sublist ((x . y)) :in '((a . b) (c . d) (e . f))
        collect (list x y))
  ((a b) (c d) (e f)))

(define-test hoop.3.9
  :compile-at :execute
  (is equal
  (hoop ((:each-sublist ((x nil y)) :in '((a b c) (d e f) (g h i))
        collect (list x y))
  ((a c) (d f) (g i)))

(define-test hoop.3.10
  :compile-at :execute
  (is equal
  (hoop ((:each-sublist ((x y)) of-type (fixnum) :in '((1 2) (3 4) (5 6))
        collect (+ x y))
  (3 7 11))

(define-test hoop.3.11
  :compile-at :execute
  (is equal
  (hoop ((:each-sublist ((x y)) of-type (fixnum) :in '((1 2) (3 4) (5 6))
        collect (+ x y))
  (3 7 11))

(define-test hoop.3.12
  :compile-at :execute
  (is equal
  (hoop ((:each-sublist ((x y)) of-type ((fixnum fixnum)) :in '((1 2) (3 4) (5 6))
        collect (+ x y))
  (3 7 11))

(define-test hoop.3.13
  :compile-at :execute
  (is equal
  (hoop ((:each-sublist ((x . y)) of-type ((fixnum . fixnum)) :in '((1 . 2) (3 . 4) (5 . 6))
        collect (+ x y))
  (3 7 11))

(define-test hoop.3.14
  :compile-at :execute
  (fail
   (hoop ((:each-sublist x :in '(a b c)
         for x :in '(d e f) collect x)
   'program-error)
  t)

(define-test hoop.3.15
  (fail (hoop ((:each-sublist (x . x) :in '((a b) (c d)) collect x)
                 'program-error)
  t)

(define-test hoop.3.16
  (hoop ((:each-sublist nil :in nil do (return t))
  nil)

(define-test hoop.3.17
  (let ((x '(a b c)))
    (values
     x
     (hoop ((:each-sublist x :in '(d e f) collect x)
     x))
  (a b c)
  ((d e f) (e f) (f))
  (a b c))

(define-test hoop.3.18
  (hoop ((:each-sublist (x) of-type ((integer 0 10)) :in '(2 4 6 7) sum x)
  19)

;;; Tests of the 'AS' form

(define-test hoop.3.19
  (hoop as x :in '(1 2 3) sum (car x))
  6)

(define-test hoop.3.20
  (hoop as x :in '(a b c)
        as y :in '(1 2 3)
        collect (list (car x) (car y)))
  ((a 1) (b 2) (c 3)))

(define-test hoop.3.21
  (hoop as x :in '(a b c)
        for y :in '(1 2 3)
        collect (list (car x) (car y)))
  ((a 1) (b 2) (c 3)))

(define-test hoop.3.22
  (hoop ((:each-sublist x :in '(a b c)
        as y :in '(1 2 3)
        collect (list (car x) (car y)))
  ((a 1) (b 2) (c 3)))

(define-test hoop.3.23
  (let (a b (i 0))
    (values
     (hoop ((:each-sublist e :in (progn (setf a (incf i))
                           '(a b c d e f g))
           :by (progn (setf b (incf i)) #'cddr)
           collect (car e))
     a b i))
  (a c e g)
  1 2 2)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(define-test hoop.3.24
  (macrolet
   ((%m (z) z))
   (hoop ((:each-sublist x :in (expand-in-current-env (%m '(1 2 3))) sum (car x)))
  6)

(define-test hoop.3.25
  (macrolet
   ((%m (z) z))
   (hoop ((:each-sublist e :in (expand-in-current-env (%m '(a b c d e f))) :by #'cddr
         collect (car e)))
  (a c e))

(define-test hoop.3.26
  (macrolet
   ((%m (z) z))
   (hoop ((:each-sublist e :in '(a b c d e f)
         :by (expand-in-current-env (%m #'cddr))
         collect (car e)))
  (a c e))

(define-test hoop.3.27
  (macrolet
   ((%m (z) z))
   (hoop as x :in (expand-in-current-env (%m '(1 2 3))) sum (car x)))
  6)
|#
