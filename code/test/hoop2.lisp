(in-package #:hoop/test)

(define-test hoop.2.1
  :compile-at :execute
  (is equal
      6
      (hoop ((:each-item x :in '(1 2 3))
             (:sum s))
            (s x))))

(define-test hoop.2.2
  :compile-at :execute
  (is equal
      2
      (hoop ((:each-item x :in '(1 2 3 4)))
            (when (evenp x) (return x)))))

(define-test hoop.2.3
  :compile-at :execute
  (fail (hoop ((:each-item x :in '(a . b))
               (:collect c)))
        'type-error))

(define-test hoop.2.4
  :compile-at :execute
  (is equal
      '(d c b a)
      (let ((x nil))
        (hoop ((:each-item e :in '(a b c d)))
          (push e x))
        x)))

(define-test hoop.2.5
  :compile-at :execute
  (is equal
      '(a c e)
      (hoop ((:each-item e :in '(a b c d e f) :by #'cddr)
             (:collect c))
        (c e))))

(define-test hoop.2.6
  :compile-at :execute
  (is equal
      '(a c e g)
      (hoop ((:each-item e :in '(a b c d e f g) :by #'cddr)
             (:collect c))
        (c e))))

(define-test hoop.2.7
  :compile-at :execute
  (is equal
      '(a a a a a a)
      (hoop ((:each-item e :in '(a b c d e f)
              :by #'(lambda (l) (and (cdr l) (cons (car l) (cddr l)))))
             (:collect c))
        (c e))))

(define-test hoop.2.8
  :compile-at :execute
  (is equal
      '((a b) (c d) (e f))
      (hoop ((:each-item (x . y) :in '((a . b) (c . d) (e . f)))
             (:collect c))
        (c (list x y)))))

(define-test hoop.2.9
  :compile-at :execute
  (is equal
      '((a c) (d f) (g i))
      (hoop ((:each-item (x nil y) :in '((a b c) (d e f) (g h i)))
             (:collect c))
        (c (list x y)))))

(define-test hoop.2.10
  :compile-at :execute
  (is equal
      '(3 7 11)
      (hoop ((:each-item (x y) #|of-type fixnum|# :in '((1 2) (3 4) (5 6)))
             (:collect c))
        (c (+ x y)))))

(define-test hoop.2.11
  :compile-at :execute
  (is equal
      '(3 7 11)
      (hoop ((:each-item (x y) #|of-type fixnum|# :in '((1 2) (3 4) (5 6)))
             (:collect c))
        (c (+ x y)))))

(define-test hoop.2.12
  :compile-at :execute
  (is equal
      '(3 7 11)
      (hoop ((:each-item (x y) #|of-type (fixnum fixnum)|# :in '((1 2) (3 4) (5 6)))
             (:collect c))
        (c (+ x y)))))

(define-test hoop.2.13
  :compile-at :execute
  (is equal
      '(3 7 11)
      (hoop ((:each-item (x . y) #|of-type (fixnum . fixnum)|# :in '((1 . 2) (3 . 4) (5 . 6)))
             (:collect c))
        (c (+ x y)))))

#|(define-test hoop.2.14
  :compile-at :execute
  (fail (hoop ((:each-item x :in '(a b c))
               (:each-item x :in '(d e f))
               (:collect c))
          (c x))
        'program-error))

(define-test hoop.2.15
  :compile-at :execute
  (fail (hoop ((:each-item (x . x) :in '((a b) (c d)))
               (:collect c))
          (c x))
        'program-error))|#

(define-test hoop.2.16
  :compile-at :execute
  (is equal
      nil
      (hoop ((:each-item nil :in nil))
        (return t))))

(define-test hoop.2.17
  :compile-at :execute
  (is-values (let ((x '(a b c)))
               (values x
                       (hoop ((:each-item x :in '(d e f))
                              (:collect c))
                         (c (list x)))
                       x))
             (equal '(a b c))
             (equal '((d) (e) (f)))
             (equal '(a b c))))

(define-test hoop.2.18
  :compile-at :execute
  (is equal
      19
      (hoop ((:each-item x #|of-type (integer 0 10)|# :in '(2 4 6 7))
             (:sum c))
        (c x))))

;;; Tests of the 'AS' ((:each-itemm

#|(define-test hoop.2.19
  (hoop as x :in '(1 2 3) sum x)
  6)

(define-test hoop.2.20
  (hoop as x :in '(a b c)
        as y :in '(1 2 3)
        collect (list x y))
  ((a 1) (b 2) (c 3)))

(define-test hoop.2.21
  (hoop as x :in '(a b c)
        ((:each-item y :in '(1 2 3)
        collect (list x y))
  ((a 1) (b 2) (c 3)))

(define-test hoop.2.22
  (hoop ((:each-item x :in '(a b c)
        as y :in '(1 2 3)
        collect (list x y))
  ((a 1) (b 2) (c 3)))

(define-test hoop.2.23
  (let (a b (i 0))
    (values
     (hoop ((:each-item e :in (progn (setf a (incf i))
                           '(a b c d e f g))
           :by (progn (setf b (incf i)) #'cddr)
           collect e)
     a b i))
  (a c e g)
  1 2 2)


;;; Test that explicit calls to macroexpand :in sub(:each-itemms
;;; are done :in the correct environment

(define-test hoop.2.24
  (macrolet
   ((%m (z) z))
   (hoop ((:each-item x :in (expand-in-current-env (%m '(1 2 3))) sum x))
  6)

(define-test hoop.2.25
  (macrolet
   ((%m (z) z))
   (hoop ((:each-item (x . y) :in (expand-in-current-env (%m '((a . b) (c . d) (e . f))))
         collect (list x y)))
  ((a b) (c d) (e f)))

(define-test hoop.2.26
  (macrolet
   ((%m (z) z))
   (hoop as x :in (expand-in-current-env (%m '(1 2 3))) sum x))
  6)
|#
