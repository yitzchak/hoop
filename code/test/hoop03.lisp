(in-package #:hoop/test)

(define-test hoop.3.1
  :compile-at :execute
  (is equal
      6
      (hoop* ((:each-sublist x :in '(1 2 3))
              (:sum c))
        (c (car x)))))

(define-test hoop.3.2
  :compile-at :execute
  (is equal
      '(2 3 4)
      (hoop* ((:each-sublist x :in '(1 2 3 4)))
        (when (evenp (car x)) (return x)))))

(define-test hoop.3.3
  :compile-at :execute
  (is equal
      '(a b c)
      (hoop* ((:each-sublist x :in '(a b c . d))
              (:collect c))
        (c (car x)))))

(define-test hoop.3.4
  :compile-at :execute
  (is equal
      '(d c b a)
      (let ((x nil))
        (hoop* ((:each-sublist e :in '(a b c d)))
          (push (car e) x))
        x)))

(define-test hoop.3.5
  :compile-at :execute
  (is equal
      '(a c e)
      (hoop* ((:each-sublist e :in '(a b c d e f) :by #'cddr)
              (:collect c))
        (c (car e)))))

(define-test hoop.3.6
  :compile-at :execute
  (is equal
      '(a c e g)
      (hoop* ((:each-sublist e :in '(a b c d e f g) :by #'cddr)
              (:collect c))
        (c (car e)))))

(define-test hoop.3.7
  :compile-at :execute
  (is equal
      '(a a a a a a)
      (hoop* ((:each-sublist e :in '(a b c d e f)
               :by #'(lambda (l)
                       (and (cdr l) (cons (car l) (cddr l)))))
              (:collect c))
        (c (car e)))))

(define-test hoop.3.8
  :compile-at :execute
  (is equal
      '((a b) (c d) (e f))
      (hoop* ((:each-sublist ((x . y)) :in '((a . b) (c . d) (e . f)))
              (:collect c))
        (c (list x y)))))

(define-test hoop.3.9
  :compile-at :execute
  (is equal
      '((a c) (d f) (g i))
      (hoop* ((:each-sublist ((x nil y)) :in '((a b c) (d e f) (g h i)))
              (:collect c))
        (c (list x y)))))

#|(define-test hoop.3.10
:compile-at :execute
  (is equal
      '(3 7 11)
      (hoop* ((:each-sublist ((x y)) of-type (fixnum) :in '((1 2) (3 4) (5 6)))
              (:collect c))
        (c (+ x y)))))

(define-test hoop.3.11
:compile-at :execute
  (is equal
      '(3 7 11)
(hoop* ((:each-sublist ((x y)) of-type (fixnum) :in '((1 2) (3 4) (5 6))
collect (+ x y))
)

(define-test hoop.3.12
:compile-at :execute
(is equal
(hoop* ((:each-sublist ((x y)) of-type ((fixnum fixnum)) :in '((1 2) (3 4) (5 6))
collect (+ x y))
(3 7 11))

(define-test hoop.3.13
:compile-at :execute
(is equal
(hoop* ((:each-sublist ((x . y)) of-type ((fixnum . fixnum)) :in '((1 . 2) (3 . 4) (5 . 6))
collect (+ x y))
(3 7 11))|#

(define-test hoop.3.14
  :compile-at :execute
  (fail-compile (hoop* ((:each-sublist x :in '(a b c))
                        (:each-item x :in '(d e f))
                        (:collect c))
                  (c x))
                program-error))

(define-test hoop.3.15
  :compile-at :execute
  (fail-compile (hoop* ((:each-sublist (x . x) :in '((a b) (c d)))
                        (:collect c))
                  (c x))
                program-error))

(define-test hoop.3.16
  :compile-at :execute
  (false (hoop* ((:each-sublist nil :in nil))
           (return t))))

(define-test hoop.3.17
  :compile-at :execute
  (is-values (let ((x '(a b c)))
               (values x
                       (hoop* ((:each-sublist x :in '(d e f))
                               (:collect c))
                         (c x))
                       x))
             (equal '(a b c))
             (equal '((d e f) (e f) (f)))
             (equal '(a b c))))

#|(define-test hoop.3.18
(hoop* ((:each-sublist (x) of-type ((integer 0 10)) :in '(2 4 6 7) sum x)
19)|#

;;; Tests of the 'AS' form

#|(define-test hoop.3.19
(hoop* as x :in '(1 2 3) sum (car x))
6)

(define-test hoop.3.20
(hoop* as x :in '(a b c)
as y :in '(1 2 3)
collect (list (car x) (car y)))
((a 1) (b 2) (c 3)))

(define-test hoop.3.21
(hoop* as x :in '(a b c)
for y :in '(1 2 3)
collect (list (car x) (car y)))
((a 1) (b 2) (c 3)))

(define-test hoop.3.22
(hoop* ((:each-sublist x :in '(a b c)
as y :in '(1 2 3)
collect (list (car x) (car y)))
((a 1) (b 2) (c 3)))

(define-test hoop.3.23
(let (a b (i 0))
(values
(hoop* ((:each-sublist e :in (progn (setf a (incf i))
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
(hoop* ((:each-sublist x :in (expand-in-current-env (%m '(1 2 3))) sum (car x)))
6)

(define-test hoop.3.25
(macrolet
((%m (z) z))
(hoop* ((:each-sublist e :in (expand-in-current-env (%m '(a b c d e f))) :by #'cddr
collect (car e)))
(a c e))

(define-test hoop.3.26
(macrolet
((%m (z) z))
(hoop* ((:each-sublist e :in '(a b c d e f)
:by (expand-in-current-env (%m #'cddr))
collect (car e)))
(a c e))

(define-test hoop.3.27
(macrolet
((%m (z) z))
(hoop* as x :in (expand-in-current-env (%m '(1 2 3))) sum (car x)))
6)
|#
