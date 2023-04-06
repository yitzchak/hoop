(in-package #:hoop/test)

(define-test hoop.10.1
  :compile-at :execute
  (is equal
      4
      (hoop* ((:step x :from 1 :to 10)
              (:count c))
        (c (< x 5)))))

(define-test hoop.10.2
  :compile-at :execute
  (is equal
      6
      (hoop* ((:step x :from 1 :to 10)
              (:count c))
        (c (< x 7)))))

(define-test hoop.10.3
  :compile-at :execute
  (is equal
      4
      (hoop* ((:step x :from 1 :to 10)
              (:count c))
        (declare (type fixnum c))
        (c (< x 5)))))

(define-test hoop.10.4
  :compile-at :execute
  (is equal
      4
      (hoop* ((:step x :from 1 :to 10)
              (:count c))
        (declare (type integer c))
        (c (< x 5)))))

(define-test hoop.10.7
  :compile-at :execute
  (is equal
      4
      (hoop* ((:step x :from 1 :to 10)
              (:count c))
        (declare (type (integer 0 100) c))
        (c (< x 5)))))

(define-test hoop.10.8
  :compile-at :execute
  (is equal
      4.0
      (hoop* ((:step x :from 1 :to 10)
              (:count c))
        (declare (type float c))
        (c (< x 5)))))

(define-test hoop.10.9
  :compile-at :execute
  (fail-compile (hoop* ((:with foo := 10)
                        (:each-item x :in '(a b c))
                        (:count foo))
                  (foo x))
                program-error))

(declaim (special *hoop-count-var*))

(define-test hoop.10.11
  :compile-at :execute
  (is-values (let ((*hoop-count-var* 100))
               (values (hoop* ((:each-item x :in '(a b c d))
                               (:count *hoop-count-var*))
                         (*hoop-count-var* x))
                       *hoop-count-var*))
             (equal 4)
             (equal 100)))

(define-test hoop.10.12
  :compile-at :execute
  (is equal
      '(1 2 2 3 3 4)
      (hoop* ((:each-item x :in '(a b nil d nil e))
              (:collect foo)
              (:count bar))
        (bar x)
        (foo bar))))

(define-test hoop.10.14
  :compile-at :execute
  (is equal
      10
      (hoop* ((:each-item x :in '(a b c))
              (:count c))
        (c (return 10)))))

;;; Tests of MAXIMIZE, MAXIMIZING

(define-test hoop.10.20
  :compile-at :execute
  (is equal
      10
      (hoop* ((:each-item x :in '(1 4 10 5 7 9))
              (:maximize c))
        (c x))))

(define-test hoop.10.22
  :compile-at :execute
  (is equal
      1000000000000
      (hoop* ((:each-item x :in '(1000000000000))
              (:maximize c))
        (c x))))

(define-test hoop.10.23
  :compile-at :execute
  (is equal
      -1000000000000
      (hoop* ((:each-item x :in '(-1000000000000))
              (:maximize c))
        (c x))))

(define-test hoop.10.24
  :compile-at :execute
  (is equal
      3.0
      (hoop* ((:each-item x :in '(1.0 2.0 3.0 -1.0))
              (:maximize c))
        (c x))))

(define-test hoop.10.25
  :compile-at :execute
  (is equal
      24
      (hoop* ((:each-item x :in '(8 20 5 3 24 1 19 4 20 3))
              (:maximize c))
        (declare (type fixnum x)
                 (type (or null fixnum) c))
        (c x))))

(define-test hoop.10.26
  :compile-at :execute
  (is equal
      24
      (hoop* ((:each-item x :in '(8 20 5 3 24 1 19 4 20 3))
              (:maximize c))
        (declare (type integer x)
                 (type (or null integer) c))
        (c x))))

(define-test hoop.10.27
  :compile-at :execute
  (is equal
      24
      (hoop* ((:each-item x :in '(8 20 5 3 24 1 19 4 20 3))
              (:maximize c))
        (declare (type rational x)
                 (type (or null rational) c))
        (c x))))

(define-test hoop.10.30
  :compile-at :execute
  (is equal
      24
      (hoop* ((:each-item x :in '(8 20 5 3 24 1 19 4 20 3))
              (:maximize c))
        (declare (type real x)
                 (type (or null real) c))
        (c x))))

(define-test hoop.10.31
  :compile-at :execute
  (is equal
      0.24
      (hoop* ((:each-item x :in '(0.08 0.20 0.05 0.03 0.24 0.01 0.19 0.04 0.20 0.03))
              (:maximize c))
        (declare (type float x)
                 (type (or null float) c))
        (c x))))

(define-test hoop.10.32
  (is equal
      -1/24
      (hoop* ((:each-item x :in '(-1/8 -1/20 -1/5 -1/3 -1/24 -1/1 -1/19 -1/4 -1/20 -1/3))
              (:maximize c))
        (declare (type rational x)
                 (type (or null rational) c))
        (c x))))

(declaim (special *hoop-max-var*))

(define-test hoop.10.36
  :compile-at :execute
  (is-values (let ((*hoop-max-var* 100))
               (values (hoop* ((:each-item x :in '(1 10 4 8))
                               (:maximize *hoop-max-var*))
                         (*hoop-max-var* x))
                       *hoop-max-var*))
             (equal 10)
             (equal 100)))

(define-test hoop.10.37
  :compile-at :execute
  (fail-compile (hoop* ((:with foo := 100)
                        (:step i :from 1 :to 10)
                        (:maximize foo))
                  (foo x))
                program-error))

(define-test hoop.10.39
    (is equal
        10
        (hoop* ((:each-item x :in '(1 2 3))
                (:maximize foo))
          (foo (return 10)))))

;;; Tests of MINIMIZE, MINIMIZING

(define-test hoop.10.40
  :compile-at :execute
  (is equal
      1
      (hoop* ((:each-item x :in '(4 10 1 5 7 9))
              (:minimize foo))
        (foo x))))

;;; hoop.10.41 is skipped because HOOP doesn't have the minimizing keyword.

(define-test hoop.10.42
  :compile-at :execute
  (is equal
      1000000000000
      (hoop* ((:each-item x :in '(1000000000000))
              (:minimize foo))
        (foo x))))

(define-test hoop.10.43
  :compile-at :execute
  (is equal
      -1000000000000
      (hoop* ((:each-item x :in '(-1000000000000))
              (:minimize foo))
        (foo x))))

(define-test hoop.10.44
  :compile-at :execute
  (is equal
      -1.0
      (hoop* ((:each-item x :in '(1.0 2.0 -1.0 3.0))
              (:minimize foo))
        (foo x))))

(define-test hoop.10.45
  :compile-at :execute
  (is equal
      1
      (hoop* ((:each-item x :in '(8 20 5 3 24 1 19 4 20 3))
              (:minimize foo))
        (declare (type fixnum x)
                 (type (or null fixnum) foo))
        (foo x))))

(define-test hoop.10.46
  :compile-at :execute
  (is equal
      1
      (hoop* ((:each-item x :in '(8 20 5 3 24 1 19 4 20 3))
              (:minimize foo))
        (declare (type integer x)
                 (type (or null integer) foo))
        (foo x))))

(define-test hoop.10.47
  :compile-at :execute
  (is equal
      1
      (hoop* ((:each-item x :in '(8 20 5 3 24 1 19 4 20 3))
              (:minimize foo))
        (declare (type rational x)
                 (type (or null rational) foo))
        (foo x))))

;;; hoop.10.48 and hoop.10.49 are skipped because HOOP always returns accumulators.

(define-test hoop.10.50
  :compile-at :execute
  (is equal
      1
      (hoop* ((:each-item x :in '(8 20 5 3 24 1 19 4 20 3))
              (:minimize foo))
        (declare (type real x)
                 (type (or null real) foo))
        (foo x))))

(define-test hoop.10.51
  :compile-at :execute
  (is equal
      0.01
      (hoop* ((:each-item x :in '(0.08 0.20 0.05 0.03 0.24 0.01 0.19 0.04 0.20 0.03))
              (:minimize foo))
        (declare (type float x)
                 (type (or null float) foo))
        (foo x))))

(define-test hoop.10.52
  :compile-at :execute
  (is equal
      -1/1
      (hoop* ((:each-item x :in '(-1/8 -1/20 -1/5 -1/3 -1/24 -1/1 -1/19 -1/4 -1/20 -1/3))
              (:minimize foo))
        (declare (type rational x)
                 (type (or null rational) foo))
        (foo x))))

;;; hoop.10.53 and hoop.10.54 are skipped because HOOP always returns accumulators.

(define-test hoop.10.55
  :compile-at :execute
  (is-values (let ((foo 20))
               (values (hoop* ((:each-item x :in '(4 5 8 3 7))
                               (:minimize foo))
                         (foo x))
                       foo))
             (equal 3)
             (equal 20)))

(declaim (special *hoop-min-var*))

(define-test hoop.10.56
  :compile-at :execute
  (is-values (let ((*hoop-min-var* 100))
               (values (hoop* ((:each-item x :in '(10 4 8))
                               (:minimize *hoop-min-var*))
                         (*hoop-min-var* x))
                       *hoop-min-var*))
             (equal 4)
             (equal 100)))

(define-test hoop.10.57
  :compile-at :execute
  (fail-compile (hoop* ((:with foo := 100)
                        (:step i :from 1 :to 10)
                        (:minimize foo))
                  (foo i))
                program-error))

;; hoop.10.58 is skipped because it is the same as hoop.10.57 under HOOP.

(define-test hoop.10.58a
  :compile-at :execute
  (is equal
      10
      (hoop* ((:each-item x :in '(1 2 3))
              (:minimize foo))
        (foo (return 10)))))

;;; Tests combining MINIMIZE, MAXIMIZE

(define-test hoop.10.59
  :compile-at :execute
  (is-values (hoop* ((:step i :from 1 :to 10)
                     (:minimize foo)
                     (:maximize bar))
               (foo i)
               (bar (- i)))
             (equal 1)
             (equal -1)))

;;; hoop.10.60 is skipped because it is covered by hoop.10.59

#+(or)(define-test hoop.10.61
  :compile-at :execute
  (is-values (hoop* ((:step i :from 5 :to 1 :by -1)
                     (:minimize foo)
                     (:maximize bar))
               (foo (- i))
               (bar i))
             (equal 1)
             (equal -1)))

;;; Tests for SUM, SUMMING

(define-test hoop.10.70
  :compile-at :execute
  (is equal
      10
      (hoop* ((:step i :from 1 :to 4)
              (:sum foo))
        (foo i))))

;;; hoop.10.71 is skipped because HOOP doesn't have summing keyword.

(define-test hoop.10.72
  :compile-at :execute
  (is equal
      10.0
      (hoop* ((:step i :from 1 :to 4)
              (:sum foo))
        (foo (float i)))))

(define-test hoop.10.73
  :compile-at :execute
  (is equal
      #C(10 10)
      (hoop* ((:step i :from 1 :to 4)
              (:sum foo))
        (foo (complex i i)))))

(define-test hoop.10.74
  :compile-at :execute
  (is equal
      10
      (hoop* ((:step i :from 1 :to 4)
              (:sum foo))
        (declare (type fixnum foo))
        (foo i))))

(define-test hoop.10.75
  :compile-at :execute
  (is equal
      10
      (hoop* ((:step i :from 1 :to 4)
              (:sum foo))
        (declare (type integer foo))
        (foo i))))

(define-test hoop.10.76
  :compile-at :execute
  (is equal
      10
      (hoop* ((:step i :from 1 :to 4)
              (:sum foo))
        (declare (type rational foo))
        (foo i))))

(define-test hoop.10.77
  :compile-at :execute
  (is equal
      10.0
      (hoop* ((:step i :from 1 :to 4)
              (:sum foo))
        (declare (type float foo))
        (foo (float i)))))

(define-test hoop.10.78
  :compile-at :execute
  (is equal
      10
      (hoop* ((:step i :from 1 :to 4)
              (:sum foo))
        (declare (type number foo))
        (foo i))))

;;; hoop.10.79 thru hoop.10.81 skipped since HOOP always return accumulators.

#|(define-test hoop.10.82
(hoop* for i from 1 to 4
sum i fixnum
count t)
14)

(define-test hoop.10.83
(hoop* for i from 1 to 4
sum i fixnum
count t fixnum)
14)

(define-test hoop.10.84
(let ((foo 100))
(values
(hoop* for i from 1 to 4 sum i into foo of-type integer
finally (return foo))
foo))
10 100)

(define-test hoop.10.85
(signals-error
(hoop* with foo = 100
for i from 1 to 4 sum i into foo
finally (return foo))
program-error)
t)

(define-test hoop.10.86
(signals-error
(hoop* with foo = 100
for i from 1 to 4 summing i into foo
finally (return foo))
program-error)
t)

(define-test hoop.10.87
(hoop* for i from 1 to 4
sum (complex i (1+ i)) of-type complex)
#c(10 14))

(define-test hoop.10.88
(hoop* for i from 1 to 4
sum (/ i 17) of-type rational)
10/17)

(define-test hoop.10.89
(hoop* for i from 1 to 4 summing (/ i 17))
10/17)

(define-test hoop.10.90
(hoop* for i from 1 to 4
sum i into foo
sum (1+ i) into bar
finally (return (values foo bar)))
10 14)

(define-test hoop.10.91
(hoop* for i from 1 to 4
sum i into foo fixnum
sum (float (1+ i)) into bar float
finally (return (values foo bar)))
10 14.0)

(define-test hoop.10.92
(hoop* for i from 1 to 4 sum (return 100))
100)

(define-test hoop.10.93
(hoop* for i from 1 to 4 summing (return 100))
100)

(define-test hoop.10.94
(hoop* for i in nil sum i of-type integer)
0)

(define-test hoop.10.95
(hoop* for i in nil sum i of-type fixnum)
0)

(define-test hoop.10.96
(hoop* for i in nil sum i of-type bit)
0)

(define-test hoop.10.97
(hoop* for i in nil sum i of-type (integer 0 100))
0)

(define-test hoop.10.98
(hoop* for i in nil sum i of-type (integer -100 0))
0)

(define-test hoop.10.99
(hoop* for i in nil sum i of-type (integer -100 100))
0)

(define-test hoop.10.100
(hoop* for i in nil sum i of-type (and integer (real -100.0 100.0)))
0)

(define-test hoop.10.101
(hoop* for i in nil sum i of-type short-float)
0.0s0)

(define-test hoop.10.102
(hoop* for i in nil sum i of-type single-float)
0.0f0)

(define-test hoop.10.103
(hoop* for i in nil sum i of-type double-float)
0.0d0)

(define-test hoop.10.104
(hoop* for i in nil sum i of-type long-float)
0.0l0)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(define-test hoop.10.105
(macrolet
((%m (z) z))
(hoop* for x from 1 to 10 count (expand-in-current-env (%m (< x 5)))))
4)

(define-test hoop.10.106
(macrolet
((%m (z) z))
(hoop* for x from 1 to 10 counting (expand-in-current-env (%m t))))
10)

(define-test hoop.10.107
(macrolet
((%m (z) z))
(hoop* for x from 1 to 10 count (expand-in-current-env (%m nil))))
0)

(define-test hoop.10.108
(macrolet
((%m (z) z))
(hoop* for x in '(1 4 10 5 7 9) maximize (expand-in-current-env (%m x))))
10)

(define-test hoop.10.109
(macrolet
((%m (z) z))
(hoop* for x in '(1 4 10 5 7 9) maximizing (expand-in-current-env (%m 17))))
17)

(define-test hoop.10.110
(macrolet
((%m (z) z))
(hoop* for x in '(5 4 10 1 7 9) minimize (expand-in-current-env (%m x))))
1)

(define-test hoop.10.111
(macrolet
((%m (z) z))
(hoop* for x in '(5 4 10 1 7 9) minimizing (expand-in-current-env (%m 3))))
3)

(define-test hoop.10.112
(macrolet
((%m (z) z))
(hoop* for x in '(1 4 10 5 7 9) sum (expand-in-current-env (%m x))))
36)

(define-test hoop.10.113
(macrolet
((%m (z) z))
(hoop* for x in '(1 4 10 5 7 9) summing (expand-in-current-env (%m 2))))
12)
|#
