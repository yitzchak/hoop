(in-package #:hoop/test)

#|
(define-test hoop.10.1
  (hoop for x from 1 to 10 count (< x 5))
  4)

(define-test hoop.10.2
  (hoop for x from 1 to 10 counting (< x 7))
  6)

(define-test hoop.10.3
  (hoop for x from 1 to 10 count (< x 5) fixnum)
  4)

(define-test hoop.10.4
  (hoop for x from 1 to 10 count (< x 5) of-type integer)
  4)

(define-test hoop.10.5
  (let (z)
    (values
     (hoop for x from 1 to 10 count (< x 5) into foo
           finally (setq z foo))
     z))
  nil
  4)

(define-test hoop.10.6
  (let (z)
    (values
     (hoop for x from 1 to 10 count (< x 5) into foo fixnum
           finally (setq z foo))
     z))
  nil
  4)

(define-test hoop.10.7
  (let (z)
    (values
     (hoop for x from 1 to 10 count (< x 5) into foo of-type (integer 0 100)
           finally (setq z foo))
     z))
  nil
  4)

(define-test hoop.10.8
  (let (z)
    (values
     (hoop for x from 1 to 10 count (< x 5) into foo float
           finally (setq z foo))
     z))
  nil
  4.0)

(define-test hoop.10.9
  (signals-error
   (hoop with foo = 10
         for x in '(a b c) count x into foo
         finally (return foo))
   program-error)
  t)

(define-test hoop.10.10
  (signals-error
   (hoop with foo = 10
         for x in '(a b c) counting x into foo
         finally (return foo))
   program-error)
  t)

(declaim (special *hoop-count-var*))

(define-test hoop.10.11
  (let ((*hoop-count-var* 100))
    (values
     (hoop for x in '(a b c d) count x into *hoop-count-var*
           finally (return *hoop-count-var*))
     *hoop-count-var*))
  4 100)

(define-test hoop.10.12
  (hoop for x in '(a b nil d nil e)
        count x into foo
        collect foo)
  (1 2 2 3 3 4))

(define-test hoop.10.13
  (hoop for x in '(a b nil d nil e)
        counting x into foo
        collect foo)
  (1 2 2 3 3 4))

(define-test hoop.10.14
  (hoop for x in '(a b c) count (return 10))
  10)


;;; Tests of MAXIMIZE, MAXIMIZING

(define-test hoop.10.20
  (hoop for x in '(1 4 10 5 7 9) maximize x)
  10)

(define-test hoop.10.21
  (hoop for x in '(1 4 10 5 7 9) maximizing x)
  10)

(define-test hoop.10.22
  (hoop for x in '(1000000000000) maximizing x)
  1000000000000)

(define-test hoop.10.23
  (hoop for x in '(-1000000000000) maximize x)
  -1000000000000)

(define-test hoop.10.24
  (hoop for x in '(1.0 2.0 3.0 -1.0) maximize x)
  3.0)

(define-test hoop.10.25
  (hoop for x in '(8 20 5 3 24 1 19 4 20 3) maximize x fixnum)
  24)

(define-test hoop.10.26
  (hoop for x in '(8 20 5 3 24 1 19 4 20 3) maximize x of-type integer)
  24)

(define-test hoop.10.27
  (hoop for x in '(8 20 5 3 24 1 19 4 20 3) maximize x of-type rational)
  24)

(define-test hoop.10.28
  (hoop for x in '(1 4 10 5 7 9) maximize x into foo finally (return foo))
  10)

(define-test hoop.10.29
  (let (z)
    (values
     (hoop for x in '(1 4 10 5 7 9) maximize x into foo finally (setq z foo))
     z))
  nil
  10)

(define-test hoop.10.30
  (hoop for x in '(8 20 5 3 24 1 19 4 20 3) maximize x of-type real)
  24)

(define-test hoop.10.31
  (hoop for x in '(0.08 0.20 0.05 0.03 0.24 0.01 0.19 0.04 0.20 0.03) maximize x of-type float)
  0.24)

(define-test hoop.10.32
  (hoop for x in '(-1/8 -1/20 -1/5 -1/3 -1/24 -1/1 -1/19 -1/4 -1/20 -1/3) maximize x of-type rational)
  -1/24)

(define-test hoop.10.33
  (hoop for x in '(1 4 10 5 7 9) maximize x into foo fixnum finally (return foo))
  10)

(define-test hoop.10.34
  (hoop for x in '(1 4 10 5 7 9) maximize x into foo of-type integer finally (return foo))
  10)

(define-test hoop.10.35
  (let ((foo 20))
    (values
     (hoop for x in '(3 5 8 3 7) maximize x into foo finally (return foo))
     foo))
  8 20)

(declaim (special *hoop-max-var*))

(define-test hoop.10.36
  (let ((*hoop-max-var* 100))
    (values
     (hoop for x in '(1 10 4 8) maximize x into *hoop-max-var*
           finally (return *hoop-max-var*))
     *hoop-max-var*))
  10 100)

(define-test hoop.10.37
  (signals-error
   (hoop with foo = 100
         for i from 1 to 10 maximize i into foo
         finally (return foo))
   program-error)
  t)

(define-test hoop.10.38
  (signals-error
   (hoop with foo = 100
         for i from 1 to 10 maximizing i into foo
         finally (return foo))
   program-error)
  t)


(define-test hoop.10.39
  (hoop for x in '(1 2 3) maximize (return 10))
  10)

;;; Tests of MINIMIZE, MINIMIZING

(define-test hoop.10.40
  (hoop for x in '(4 10 1 5 7 9) minimize x)
  1)

(define-test hoop.10.41
  (hoop for x in '(4 10 5 7 1 9) minimizing x)
  1)

(define-test hoop.10.42
  (hoop for x in '(1000000000000) minimizing x)
  1000000000000)

(define-test hoop.10.43
  (hoop for x in '(-1000000000000) minimize x)
  -1000000000000)

(define-test hoop.10.44
  (hoop for x in '(1.0 2.0 -1.0 3.0) minimize x)
  -1.0)

(define-test hoop.10.45
  (hoop for x in '(8 20 5 3 24 1 19 4 20 3) minimize x fixnum)
  1)

(define-test hoop.10.46
  (hoop for x in '(8 20 5 3 24 1 19 4 20 3) minimize x of-type integer)
  1)

(define-test hoop.10.47
  (hoop for x in '(8 20 5 3 24 1 19 4 20 3) minimize x of-type rational)
  1)

(define-test hoop.10.48
  (hoop for x in '(1 4 10 5 7 9) minimize x into foo finally (return foo))
  1)

(define-test hoop.10.49
  (let (z)
    (values
     (hoop for x in '(4 1 10 1 5 7 9) minimize x into foo finally (setq z foo))
     z))
  nil
  1)

(define-test hoop.10.50
  (hoop for x in '(8 20 5 3 24 1 19 4 20 3) minimize x of-type real)
  1)

(define-test hoop.10.51
  (hoop for x in '(0.08 0.40 0.05 0.03 0.44 0.01 0.19 0.04 0.40 0.03) minimize x of-type float)
  0.01)

(define-test hoop.10.52
  (hoop for x in '(-1/8 -1/20 -1/5 -1/3 -1/24 -1/1 -1/19 -1/4 -1/20 -1/3) minimize x of-type rational)
  -1/1)

(define-test hoop.10.53
  (hoop for x in '(4 10 5 1 7 9) minimize x into foo fixnum finally (return foo))
  1)

(define-test hoop.10.54
  (hoop for x in '(1 4 10 5 7 9) minimize x into foo of-type integer finally (return foo))
  1)

(define-test hoop.10.55
  (let ((foo 20))
    (values
     (hoop for x in '(4 5 8 3 7) minimize x into foo finally (return foo))
     foo))
  3 20)

(declaim (special *hoop-min-var*))

(define-test hoop.10.56
  (let ((*hoop-min-var* 100))
    (values
     (hoop for x in '(10 4 8) minimize x into *hoop-min-var*
           finally (return *hoop-min-var*))
     *hoop-min-var*))
  4 100)

(define-test hoop.10.57
  (signals-error
   (hoop with foo = 100
         for i from 1 to 10 minimize i into foo
         finally (return foo))
   program-error)
  t)

(define-test hoop.10.58
  (signals-error
   (hoop with foo = 100
         for i from 1 to 10 minimizing i into foo
         finally (return foo))
   program-error)
  t)

(define-test hoop.10.58a
  (hoop for x in '(1 2 3) minimize (return 10))
  10)

;;; Tests combining MINIMIZE, MAXIMIZE

(define-test hoop.10.59
  (hoop for i from 1 to 10
        minimize i
        maximize (- i))
  1)

(define-test hoop.10.60
  (hoop for i from 1 to 10
        maximize (- i)
        minimize i)
  -1)

(define-test hoop.10.61
  (hoop for i from 5 downto 1
        maximize i
        minimize (- i))
  -1)


;;; Tests for SUM, SUMMING

(define-test hoop.10.70
  (hoop for i from 1 to 4 sum i)
  10)

(define-test hoop.10.71
  (hoop for i from 1 to 4 summing i)
  10)

(define-test hoop.10.72
  (hoop for i from 1 to 4 sum (float i))
  10.0)

(define-test hoop.10.73
  (hoop for i from 1 to 4 sum (complex i i))
  #c(10 10))

(define-test hoop.10.74
  (hoop for i from 1 to 4 sum i fixnum)
  10)

(define-test hoop.10.75
  (hoop for i from 1 to 4 sum i of-type integer)
  10)

(define-test hoop.10.76
  (hoop for i from 1 to 4 sum i of-type rational)
  10)

(define-test hoop.10.77
  (hoop for i from 1 to 4 sum (float i) float)
  10.0)

(define-test hoop.10.78
  (hoop for i from 1 to 4 sum i of-type number)
  10)

(define-test hoop.10.79
  (hoop for i from 1 to 4 sum i into foo finally (return foo))
  10)

(define-test hoop.10.80
  (hoop for i from 1 to 4 sum i into foo fixnum finally (return foo))
  10)

(define-test hoop.10.81
  (let (z)
    (values
     (hoop for i from 1 to 4 sum i into foo of-type (integer 0 10)
           finally (setq z foo))
     z))
  nil
  10)

(define-test hoop.10.82
  (hoop for i from 1 to 4
        sum i fixnum
        count t)
  14)

(define-test hoop.10.83
  (hoop for i from 1 to 4
        sum i fixnum
        count t fixnum)
  14)

(define-test hoop.10.84
  (let ((foo 100))
    (values
     (hoop for i from 1 to 4 sum i into foo of-type integer
           finally (return foo))
     foo))
  10 100)

(define-test hoop.10.85
  (signals-error
   (hoop with foo = 100
         for i from 1 to 4 sum i into foo
         finally (return foo))
   program-error)
  t)

(define-test hoop.10.86
  (signals-error
   (hoop with foo = 100
         for i from 1 to 4 summing i into foo
         finally (return foo))
   program-error)
  t)

(define-test hoop.10.87
  (hoop for i from 1 to 4
        sum (complex i (1+ i)) of-type complex)
  #c(10 14))

(define-test hoop.10.88
  (hoop for i from 1 to 4
        sum (/ i 17) of-type rational)
  10/17)

(define-test hoop.10.89
  (hoop for i from 1 to 4 summing (/ i 17))
  10/17)

(define-test hoop.10.90
  (hoop for i from 1 to 4
        sum i into foo
        sum (1+ i) into bar
        finally (return (values foo bar)))
  10 14)

(define-test hoop.10.91
  (hoop for i from 1 to 4
        sum i into foo fixnum
        sum (float (1+ i)) into bar float
        finally (return (values foo bar)))
  10 14.0)

(define-test hoop.10.92
  (hoop for i from 1 to 4 sum (return 100))
  100)

(define-test hoop.10.93
  (hoop for i from 1 to 4 summing (return 100))
  100)

(define-test hoop.10.94
  (hoop for i in nil sum i of-type integer)
  0)

(define-test hoop.10.95
  (hoop for i in nil sum i of-type fixnum)
  0)

(define-test hoop.10.96
  (hoop for i in nil sum i of-type bit)
  0)

(define-test hoop.10.97
  (hoop for i in nil sum i of-type (integer 0 100))
  0)

(define-test hoop.10.98
  (hoop for i in nil sum i of-type (integer -100 0))
  0)

(define-test hoop.10.99
  (hoop for i in nil sum i of-type (integer -100 100))
  0)

(define-test hoop.10.100
  (hoop for i in nil sum i of-type (and integer (real -100.0 100.0)))
  0)

(define-test hoop.10.101
  (hoop for i in nil sum i of-type short-float)
  0.0s0)

(define-test hoop.10.102
  (hoop for i in nil sum i of-type single-float)
  0.0f0)

(define-test hoop.10.103
  (hoop for i in nil sum i of-type double-float)
  0.0d0)

(define-test hoop.10.104
  (hoop for i in nil sum i of-type long-float)
  0.0l0)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(define-test hoop.10.105
  (macrolet
   ((%m (z) z))
   (hoop for x from 1 to 10 count (expand-in-current-env (%m (< x 5)))))
  4)

(define-test hoop.10.106
  (macrolet
   ((%m (z) z))
   (hoop for x from 1 to 10 counting (expand-in-current-env (%m t))))
  10)

(define-test hoop.10.107
  (macrolet
   ((%m (z) z))
   (hoop for x from 1 to 10 count (expand-in-current-env (%m nil))))
  0)

(define-test hoop.10.108
  (macrolet
   ((%m (z) z))
   (hoop for x in '(1 4 10 5 7 9) maximize (expand-in-current-env (%m x))))
  10)

(define-test hoop.10.109
  (macrolet
   ((%m (z) z))
   (hoop for x in '(1 4 10 5 7 9) maximizing (expand-in-current-env (%m 17))))
  17)

(define-test hoop.10.110
  (macrolet
   ((%m (z) z))
   (hoop for x in '(5 4 10 1 7 9) minimize (expand-in-current-env (%m x))))
  1)

(define-test hoop.10.111
  (macrolet
   ((%m (z) z))
   (hoop for x in '(5 4 10 1 7 9) minimizing (expand-in-current-env (%m 3))))
  3)

(define-test hoop.10.112
  (macrolet
   ((%m (z) z))
   (hoop for x in '(1 4 10 5 7 9) sum (expand-in-current-env (%m x))))
  36)

(define-test hoop.10.113
  (macrolet
   ((%m (z) z))
   (hoop for x in '(1 4 10 5 7 9) summing (expand-in-current-env (%m 2))))
  12)
|#
