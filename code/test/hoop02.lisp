(in-package #:hoop/test)

(define-test hoop.2.1
  :compile-at :execute
  (is equal
      6
      (hoop* ((:each-item x :in '(1 2 3))
              (:sum s))
        (s x))))

(define-test hoop.2.2
  :compile-at :execute
  (is equal
      2
      (hoop* ((:each-item x :in '(1 2 3 4)))
        (when (evenp x) (return x)))))

(define-test hoop.2.3
  :compile-at :execute
  (fail (hoop* ((:each-item x :in '(a . b))
                (:collect c)))
        'type-error))

(define-test hoop.2.4
  :compile-at :execute
  (is equal
      '(d c b a)
      (let ((x nil))
        (hoop* ((:each-item e :in '(a b c d)))
          (push e x))
        x)))

(define-test hoop.2.5
  :compile-at :execute
  (is equal
      '(a c e)
      (hoop* ((:each-item e :in '(a b c d e f) :by #'cddr)
              (:collect c))
        (c e))))

(define-test hoop.2.6
  :compile-at :execute
  (is equal
      '(a c e g)
      (hoop* ((:each-item e :in '(a b c d e f g) :by #'cddr)
              (:collect c))
        (c e))))

(define-test hoop.2.7
  :compile-at :execute
  (is equal
      '(a a a a a a)
      (hoop* ((:each-item e :in '(a b c d e f)
               :by #'(lambda (l) (and (cdr l) (cons (car l) (cddr l)))))
              (:collect c))
        (c e))))

(define-test hoop.2.8
  :compile-at :execute
  (is equal
      '((a b) (c d) (e f))
      (hoop* ((:each-item (x . y) :in '((a . b) (c . d) (e . f)))
              (:collect c))
        (c (list x y)))))

(define-test hoop.2.9
  :compile-at :execute
  (is equal
      '((a c) (d f) (g i))
      (hoop* ((:each-item (x nil y) :in '((a b c) (d e f) (g h i)))
              (:collect c))
        (c (list x y)))))

(define-test hoop.2.10
  :compile-at :execute
  (is equal
      '(3 7 11)
      (hoop* ((:each-item (x y) :in '((1 2) (3 4) (5 6)))
              (:collect c))
        (declare (type fixnum x y))
        (c (+ x y)))))

(define-test hoop.2.13
  :compile-at :execute
  (is equal
      '(3 7 11)
      (hoop* ((:each-item (x . y) :in '((1 . 2) (3 . 4) (5 . 6)))
              (:collect c))
        (declare (type fixnum x y))
        (c (+ x y)))))

(define-test hoop.2.14
  :compile-at :execute
  (fail (macroexpand-1 '(hoop* ((:each-item x :in '(a b c))
                                (:each-item x :in '(d e f))
                                (:collect c))
                         (c x)))
        'program-error))

(define-test hoop.2.15
  :compile-at :execute
  (fail (macroexpand-1 '(hoop* ((:each-item (x . x) :in '((a b) (c d)))
                                (:collect c))
                         (c x)))
        'program-error))

(define-test hoop.2.16
  :compile-at :execute
  (is equal
      nil
      (hoop* ((:each-item nil :in nil))
        (return t))))

(define-test hoop.2.17
  :compile-at :execute
  (is-values (let ((x '(a b c)))
               (values x
                       (hoop* ((:each-item x :in '(d e f))
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
      (hoop* ((:each-item x :in '(2 4 6 7))
              (:sum c))
        (declare (type (integer 0 10) x))
        (c x))))

;;; hoop.2.19 thru hoop.2.23 are skipped because HOOP doesn't have AS

;;; Test that explicit calls to macroexpand :in sub(:each-itemms
;;; are done :in the correct environment

(define-test hoop.2.24
  :compile-at :execute
  (is equal
      6
      (macrolet ((%m (z) z))
        (hoop* ((:each-item x :in (expand-in-current-env (%m '(1 2 3))))
                (:sum foo))
          (foo x)))))

(define-test hoop.2.25
  :compile-at :execute
  (is equal
      '((a b) (c d) (e f))
      (macrolet ((%m (z) z))
        (hoop* ((:each-item (x . y) :in (expand-in-current-env (%m '((a . b) (c . d) (e . f)))))
                (:collect foo))
          (foo (list x y))))))

;;; hoop.2.26 is skipped since HOOP doesn't have AS
