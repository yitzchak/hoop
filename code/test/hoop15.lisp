(in-package #:hoop/test)

#|
(define-test hoop.15.30
(hoop* :for i :from 1 :to 10 :collect i)
(1 2 3 4 5 6 7 8 9 10))

(define-test hoop.15.31
(hoop* :for i :upfrom 1 :below 10 :by 2 :collect i)
(1 3 5 7 9))

(define-test hoop.15.32
(hoop* :with x = 1 :and y = 2 :return (values x y))
1 2)

(define-test hoop.15.33
(hoop* :named foo :doing (return-from foo 1))
1)

(define-test hoop.15.34
(let ((x 0))
(hoop
:initially (setq x 2)
:until t
:finally (return x)))
2)

(define-test hoop.15.35
(hoop* :for x :in '(a b c) :collecting x)
(a b c))

(define-test hoop.15.36
(hoop* :for x :in '(a b c) :append (list x))
(a b c))

(define-test hoop.15.37
(hoop* :for x :in '(a b c) :appending (list x))
(a b c))

(define-test hoop.15.38
(hoop* :for x :in '(a b c) :nconc (list x))
(a b c))

(define-test hoop.15.39
(hoop* :for x :in '(a b c) :nconcing (list x))
(a b c))

(define-test hoop.15.40
(hoop* :for x :in '(1 2 3) :count x)
3)

(define-test hoop.15.41
(hoop* :for x :in '(1 2 3) :counting x)
3)

(define-test hoop.15.42
(hoop* :for x :in '(1 2 3) :sum x)
6)

(define-test hoop.15.43
(hoop* :for x :in '(1 2 3) :summing x)
6)

(define-test hoop.15.44
(hoop* :for x :in '(10 20 30) :maximize x)
30)

(define-test hoop.15.45
(hoop* :for x :in '(10 20 30) :maximizing x)
30)

(define-test hoop.15.46
(hoop* :for x :in '(10 20 30) :minimize x)
10)

(define-test hoop.15.47
(hoop* :for x :in '(10 20 30) :minimizing x)
10)

(define-test hoop.15.48
(hoop* :for x :in '(1 2 3 4) :sum x :into foo :of-type fixnum
:finally (return foo))
10)

(define-test hoop.15.49
(hoop* :for x :upfrom 1 :to 10
:if (evenp x) :sum x :into foo
:else :sum x :into bar
:end
:finally (return (values foo bar)))
30 25)

(define-test hoop.15.50
(hoop* :for x :downfrom 10 :above 0
:when (evenp x) :sum x :into foo
:else :sum x :into bar
:end
:finally (return (values foo bar)))
30 25)

(define-test hoop.15.51
(hoop* :for x :in '(a b nil c d nil)
:unless x :count t)
2)

(define-test hoop.15.52
(hoop* :for x :in '(a b nil c d nil)
:unless x :collect x :into bar :and :count t :into foo
:end
finally (return (values bar foo)))
(nil nil)
2)

(define-test hoop.15.53
(hoop* :for x :in '(nil nil a b nil c nil)
:collect x
:until x)
(nil nil a))

(define-test hoop.15.54
(hoop* :for x :in '(a b nil c nil)
:while x :collect x)
(a b))

(define-test hoop.15.55
(hoop* :for x :in '(nil nil a b nil c nil)
:thereis x)
a)

(define-test hoop.15.56
(hoop* :for x :in '(nil nil a b nil c nil)
:never x)
nil)

(define-test hoop.15.57
(hoop* :for x :in '(a b c d e)
:always x)
t)

(define-test hoop.15.58
(hoop* :as x :in '(a b c) :count t)
3)

(define-test hoop.15.59
(hoop* :for i :from 10 :downto 5 :collect i)
(10 9 8 7 6 5))

(define-test hoop.15.60
(hoop* :for i :from 0 :upto 5 :collect i)
(0 1 2 3 4 5))

(define-test hoop.15.61
(hoop* :for x :on '(a b c) :collecting (car x))
(a b c))

(define-test hoop.15.62
(hoop* :for x = '(a b c) :then (cdr x)
:while x
:collect (car x))
(a b c))

(define-test hoop.15.63
(hoop* :for x :across #(a b c) :collect x)
(a b c))

(define-test hoop.15.64
(hoop* :for x :being :the :hash-keys :of (make-hash-table)
:count t)
0)

(define-test hoop.15.65
(hoop* :for x :being :each :hash-key :in (make-hash-table)
:count t)
0)

(define-test hoop.15.66
(hoop* :for x :being :each :hash-value :of (make-hash-table)
:count t)
0)

(define-test hoop.15.67
(hoop* :for x :being :the :hash-values :in (make-hash-table)
:count t)
0)

(define-test hoop.15.68
(hoop* :for x :being :the :hash-values :in (make-hash-table)
:using (:hash-key k)
:count t)
0)

(define-test hoop.15.69
(hoop* :for x :being :the :hash-keys :in (make-hash-table)
:using (:hash-value v)
:count t)
0)

(define-test hoop.15.70
(let ()
(ignore-errors (delete-package "LOOP.15.PACKAGE"))
(let ((p (make-package "LOOP.15.PACKAGE" :use nil)))
(hoop* :for x :being :the :symbols :of p :count t)))
0)

(define-test hoop.15.71
(let ()
(ignore-errors (delete-package "LOOP.15.PACKAGE"))
(let ((p (make-package "LOOP.15.PACKAGE" :use nil)))
(hoop* :for x :being :each :symbol :of p :count t)))
0)

(define-test hoop.15.72
(let ()
(ignore-errors (delete-package "LOOP.15.PACKAGE"))
(let ((p (make-package "LOOP.15.PACKAGE" :use nil)))
(hoop* :for x :being :the :external-symbols :of p :count t)))
0)

(define-test hoop.15.73
(let ()
(ignore-errors (delete-package "LOOP.15.PACKAGE"))
(let ((p (make-package "LOOP.15.PACKAGE" :use nil)))
(hoop* :for x :being :each :external-symbol :of p :count t)))
0)

(define-test hoop.15.74
(let ()
(ignore-errors (delete-package "LOOP.15.PACKAGE"))
(let ((p (make-package "LOOP.15.PACKAGE" :use nil)))
(hoop* :for x :being :the :present-symbols :of p :count t)))
0)

(define-test hoop.15.75
(let ()
(ignore-errors (delete-package "LOOP.15.PACKAGE"))
(let ((p (make-package "LOOP.15.PACKAGE" :use nil)))
(hoop* :for x :being :each :present-symbol :of p :count t)))
0)
|#
