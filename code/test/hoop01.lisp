(cl:in-package #:hoop/test)

(define-test hoop.1.1
  :compile-at :execute
  (is equal
      '(1 2 3 4 5 6 7 8 9 10)            
      (hoop* ((:step x :from 1 :to 10)
              (:collect c))
        (c x))))

(define-test hoop.1.2
  :compile-at :execute
  (is equal
      '(6 5 4 3 2 1)
      (hoop* ((:step x :from 6 :to 1 :by -1)
              (:collect c))
        (c x))))

(define-test hoop.1.3
  :compile-at :execute
  (is equal
      '(1)
      (hoop* ((:step x :from 1 :to 1)
              (:collect c))
        (c x))))

(define-test hoop.1.4
  :compile-at :execute
  (is equal
      nil
      (hoop* ((:step x :from 1 :to 0)
              (:collect c))
        (c x))))

(define-test hoop.1.5
  :compile-at :execute
  (is equal
      '(0 1 2 3 4 5)
      (hoop* ((:step x :to 5)
              (:collect c))
        (c x))))

(define-test hoop.1.6
  :compile-at :execute
  (is equal
      '(5 4 3 2 1 0)
      (hoop* ((:step x :from 5 :to 0 :by -1)
              (:collect c))
        (c x))))

(define-test hoop.1.7
  :compile-at :execute
  (is equal
      '(1 2 3 4 5)
      (hoop* ((:step x :from 1 :to 5)
              (:collect c))
        (c x))))

(define-test hoop.1.8
  :compile-at :execute
  (is equal
      5
      (hoop* ((:step x :from 1.0 :to 5.0)
              (:count c))
        (c x))))

(define-test hoop.1.9
  :compile-at :execute
  (is equal
      '(1 3 5 7 9)
      (hoop* ((:step x :from 1 :to 9 :by 2)
              (:collect c))
        (c x))))

(define-test hoop.1.10
  :compile-at :execute
  (is equal
      '(1 3 5 7 9)
      (hoop* ((:step x :from 1 :to 10 :by 2)
              (:collect c))
        (c x))))

(define-test hoop.1.11
  :compile-at :execute
  (is equal
      '(1 2 3 4 5 6 7 8 9 10)
      (hoop* ((:step x :to 10 :from 1)
              (:collect c))
        (c x))))

(define-test hoop.1.12
  :compile-at :execute
  (is equal
      '(1 3 5 7 9)
      (hoop* ((:step x :to 10 :by 2 :from 1)
              (:collect c))
        (c x))))

(define-test hoop.1.13
  :compile-at :execute
  (is equal
      '(1 3 5 7 9)
      (hoop* ((:step x :by 2 :to 10 :from 1)
              (:collect c))
        (c x))))

(define-test hoop.1.14
  :compile-at :execute
  (is equal
      '(0 2 4 6 8 10)
      (hoop* ((:step x :by 2 :to 10)
              (:collect c))
        (c x))))

(define-test hoop.1.15
  :compile-at :execute
  (is equal
      '(0 2 4 6 8 10)
      (hoop* ((:step x :to 10 :by 2)
              (:collect c))
        (c x))))

(define-test hoop.1.16
  :compile-at :execute
  (is equal
      '(1 2 3 4 5 6)
      (let ((n 0))
        (hoop* ((:step x :from (incf n) :to (+ n 5))
                (:collect c))
          (c x)))))

(define-test hoop.1.17
  :compile-at :execute
  (is equal
      '(1 2 3 4 5)
      (let ((n 0))
        (hoop* ((:step x :to (+ n 5) :from (incf n))
                (:collect c))
          (c x)))))

(define-test hoop.1.18
  :compile-at :execute
  (is equal
      '(1 3 5 7 9)
      (let ((n 0))
        (hoop* ((:step x :from (incf n) :to (+ n 9) :by (incf n))
                (:collect c))
          (c x)))))

(define-test hoop.1.19
  :compile-at :execute
  (is equal
      '(1 3 5 7 9 11)
      (let ((n 0))
        (hoop* ((:step x :from (incf n) :by (incf n) :to (+ n 9))
                (:collect c))
          (c x)))))

(define-test hoop.1.20
  :compile-at :execute
  (is equal
      '(0 1 2 3 4 5)
      (let ((a 0) (b 5) (c 1))
        (hoop* ((:step x :from a :to b :by c)
                (:collect d))
          (d (progn (incf a) (incf b 2) (incf c 3) x))))))

(define-test hoop.1.21
  :compile-at :execute
  (is equal
      '(0 1/2 1 3/2 2 5/2 3 7/2 4 9/2 5)
      (hoop* ((:step x :from 0 :to 5 :by 1/2)
              (:collect c))
        (c x))))

(define-test hoop.1.22
  :compile-at :execute
  (is equal
      '(1 2 3 4)
      (hoop* ((:step x :from 1 :before 5)
              (:collect c))
        (c x))))

(define-test hoop.1.23
  :compile-at :execute
  (is equal
      '(1 2 3 4 5)
      (hoop* ((:step x :from 1 :before 5.01)
              (:collect c))
        (c x))))

(define-test hoop.1.24
  :compile-at :execute
  (is equal
      '(2 3 4)
      (hoop* ((:step x :before 5 :from 2)
              (:collect c))
        (c x))))

(define-test hoop.1.25
  :compile-at :execute
  (is equal
      '(10 9 8 7 6 5)
      (hoop* ((:step x :from 10 :before 4 :by -1)
              (:collect c))
        (c x))))

(define-test hoop.1.26
  :compile-at :execute
  (is equal
      '(14 12 10 8)
      (hoop* ((:step x :from 14 :before 6 :by -2)
              (:collect c))
        (c x))))

(define-test hoop.1.27
  :compile-at :execute
  (is equal
      '(14 12 10 8)
      (hoop* ((:step x :before 6 :from 14 :by -2)
              (:collect c))
        (c x))))

(define-test hoop.1.28
  :compile-at :execute
  (is equal
      '(16 13 10)
      (hoop* ((:step x :from 16 :before 7 :by -3)
              (:collect c))
        (c x))))

(define-test hoop.1.29
  :compile-at :execute
  (is-values (let (a b c (i 0))
               (values (hoop* ((:step x :from (progn (setq a (incf i)) 0)
                                :before (progn (setq b (incf i)) 9)
                                :by (progn (setq c (incf i)) 2))
                               (:collect c))
                         (c x))
                       a b c i))
             (equal '(0 2 4 6 8))
             (equal 1)
             (equal 2)
             (equal 3)
             (equal 3)))

(define-test hoop.1.30
  :compile-at :execute
  (is-values (let (a b c (i 0))
               (values (hoop* ((:step x :from (progn (setq a (incf i)) 0)
                                :by (progn (setq c (incf i)) 2)
                                :before (progn (setq b (incf i)) 9))
                               (:collect c))
                         (c x))
                       a b c i))
             (equal '(0 2 4 6 8))
             (equal 1)
             (equal 3)
             (equal 2)
             (equal 3)))

(define-test hoop.1.31
  :compile-at :execute
  (is-values (let (a b c (i 0))
               (values (hoop* ((:step x
                                :before (progn (setq b (incf i)) 9)
                                :by (progn (setq c (incf i)) 2)
                                :from (progn (setq a (incf i)) 0))
                               (:collect c))
                         (c x))
                       a b c i))
             (equal '(0 2 4 6 8))
             (equal 3)
             (equal 1)
             (equal 2)
             (equal 3)))

(define-test hoop.1.32
  :compile-at :execute
  (is-values (let (a b c (i 0))
               (values (hoop* ((:step x
                                :by (progn (setq c (incf i)) 2)
                                :before (progn (setq b (incf i)) 9)
                                :from (progn (setq a (incf i)) 0))
                               (:collect c))
                         (c x))
                       a b c i))
             (equal '(0 2 4 6 8))
             (equal 3)
             (equal 2)
             (equal 1)
             (equal 3)))

(define-test hoop.1.33
  (is equal
      '(1 2 3 4 5)
      (hoop* ((:step x :from 1 :to 5)
              (:collect c))
        (c x))))

(define-test hoop.1.34
  :compile-at :execute
  (is equal
      '(1 2 3 4)
      (hoop* ((:step x :from 1 :to 4.0)
              (:collect c))
        (c x))))

(define-test hoop.1.35
  :compile-at :execute
  (is equal
      '(0 1 2 3 4)
      (hoop* ((:step x :before 5)
              (:collect c))
        (c x))))

(define-test hoop.1.36
  :compile-at :execute
  (is equal
      '(0 3 6 9 12 15 18)
      (hoop* ((:step x :before 20 :by 3)
              (:collect c))
        (c x))))

(define-test hoop.1.37
  :compile-at :execute
  (is equal
      '(0 3 6 9 12 15 18)
      (hoop* ((:step x :by 3 :before 20)
              (:collect c))
        (c x))))

(define-test hoop.1.38
  (is equal
      '(1 2 3 4 5)
      (hoop* ((:step x :from 1 :to 5)
              (:collect c))
        (declare (type fixnum x))
        (c x))))

;;; The following provides an example where an incorrect
;;; implementation will assign X an out-of-range value
;;; at the end.
(define-test hoop.1.39
  :compile-at :execute
  (is equal
      '(1 2 3 4 5)
      (hoop* ((:step x :from 1 :to 5)
              (:collect c))
        (declare (type (integer 1 5) x))
        (c x))))

;;; Test that the index variable achieves the inclusive
;;; upper bound, but does not exceed it.
(define-test hoop.1.40
  :compile-at :execute
  (is equal
      5
      (hoop* ((:step x :from 1 :to 5)
              (:epilogue (return x))))))

;;; Test that the index variable achieves the exclusive
;;; upper bound, but does not exceed it.
(define-test hoop.1.41
  :compile-at :execute
  (is equal
      4
      (hoop* ((:step x :from 1 :before 5)
              (:epilogue (return x))))))

(define-test hoop.1.42
  :compile-at :execute
  (is equal
      0
      (hoop* ((:step x :from 10 :to 0 :by -1)
              (:epilogue (return x))))))

(define-test hoop.1.43
  :compile-at :execute
  (is equal
      1
      (hoop* ((:step x :from 10 :before 0 :by -1)
              (:epilogue (return x))))))

;;; The arithmetic hoop ((:stepm says the types are numbers, not
;;; reals, so arguably they should work on complexes (which are
;;; numbers.)  Comparing these ((:step termination could be problematic,
;;; but a clause without termination should work just fine.

(define-test hoop.1.44
  :compile-at :execute
  (is equal
      '(#c(0 1) #c(1 1) #c(2 1) #c(3 1) #c(4 1))
      (hoop* ((:step i :from 1 :to 5)
              (:step c :from #c(0 1))
              (:collect d))
        (d c))))

(define-test hoop.1.45
  :compile-at :execute
  (is equal
      '(#c(0 1) #c(2 1) #c(4 1) #c(6 1) #c(8 1))
      (hoop* ((:step i :from 1 :to 5)
              (:step c :from #c(0 1) :by 2)
              (:collect d))
        (d c))))

(define-test hoop.1.46
  :compile-at :execute
  (is equal
      '(#c(5 1) #c(4 1) #c(3 1) #c(2 1) #c(1 1))
      (hoop* ((:step i :from 1 :to 5)
              (:step c :from #c(5 1) :by -1)
              (:collect d))
        (d c))))

(define-test hoop.1.47
  :compile-at :execute
  (is equal
      '(#c(10 1) #c(8 1) #c(6 1) #c(4 1) #c(2 1))
      (hoop* ((:step i :from 1 :to 5)
              (:step c :from #c(10 1) :by -2)
              (:collect d))
        (d c))))

(define-test hoop.1.48
  (is equal
      '(#c(0 1) #c(1 1) #c(2 1) #c(3 1) #c(4 1))
      (hoop* ((:step i :from 1 :to 5)
              (:step c :from #c(0 1))
              (:collect d))
        (d c))))

(define-test hoop.1.49
  (is equal
      '(#c(0 1) #c(2 1) #c(4 1) #c(6 1) #c(8 1))
      (hoop* ((:step i :from 1 :to 5)
              (:step c :from #c(0 1) :by 2)
              (:collect d))
        (d c))))

;;; The variable in the hoop ((:step-as-arithmetic clause
;;; can be a d-var-spec, so 'NIL' should mean don't bind anything

(define-test hoop.1.50
  :compile-at :execute
  (is equal
      '(1 2 3 4 5 6)
      (let ((i 0))
        (hoop* ((:step nil :from 10 :to 15)
                (:collect c))
          (c (incf i))))))

(define-test hoop.1.51
  :compile-at :execute
  (is equal
      '(1 2 3 4 5)
      (let ((i 0))
        (hoop* ((:step nil :from 10 :before 15)
                (:collect c))
          (c (incf i))))))

(define-test hoop.1.52
  :compile-at :execute
  (is equal
      nil
      (hoop* ((:step nil :from 10 :to 0)
              (:collect c))
        (c 'a))))

(define-test hoop.1.53
  :compile-at :execute
  (is equal
      '(1 2 3 4 5 6)
      (let ((i 0))
        (hoop* ((:step nil :from 0 :to 10 :by 2)
                (:collect c))
          (c (incf i))))))

(define-test hoop.1.54
  :compile-at :execute
  (is equal
      '(1 2 3 4)
      (let ((i 0))
        (hoop* ((:step nil :from 1 :to 4)
                (:step nil :from 1 :to 10)
                (:collect c))
          (c (incf i))))))

(define-test hoop.1.55
  :compile-at :execute
  (is equal
      '(1 2 3 4 5 6)
      (let ((i 0))
        (hoop* ((:step nil :from 5 :to 0 :by -1)
                
                (:collect c))
          (c (incf i))))))

(define-test hoop.1.56
  :compile-at :execute
  (is equal
      '(1 2 3 4 5)
      (let ((i 0))
        (hoop* ((:step nil :from 5 :before 0 :by -1)
                (:collect c))
          (c (incf i))))))

;;; Test that explicit calls :to macroexpand in subforms
;;; are done in the correct environment

(define-test hoop.1.57
  :compile-at :execute
  (is equal
      '(1 2 3 4 5)
      (macrolet ((%m (z) z))
        (hoop* ((:step i :from (expand-in-current-env (%m 1)) :to 5)
                (:collect foo))
          (foo i)))))

(define-test hoop.1.58
  :compile-at :execute
  (is equal
      '(1 2 3 4 5)
      (macrolet ((%m (z) z))
        (hoop* ((:step i :from 1 :to (expand-in-current-env (%m 5)))
                (:collect foo))
          (foo i)))))

(define-test hoop.1.59
  :compile-at :execute
  (is equal
      '(1 3 5)
      (macrolet ((%m (z) z))
        (hoop* ((:step i :from 1 :to 5 :by (expand-in-current-env (%m 2)))
                (:collect foo))
          (foo i)))))

(define-test hoop.1.60
  :compile-at :execute
  (is equal
      '(10 9 8 7 6 5 4 3)
      (macrolet ((%m (z) z))
        (hoop* ((:step i :from (expand-in-current-env (%m 10)) :by -1 :to 3)
                (:collect foo))
          (foo i)))))

(define-test hoop.1.61
  :compile-at :execute
  (is equal
      '(10 9 8 7 6 5 4 3)
      (macrolet ((%m (z) z))
        (hoop* ((:step i :from 10 :to (expand-in-current-env (%m 3)) :by -1)
                (:collect foo))
          (foo i)))))

(define-test hoop.1.62
  :compile-at :execute
  (is equal
      '(10 9 8 7 6 5 4 3)
      (macrolet ((%m (z) z))
        (hoop* ((:step i :from (expand-in-current-env (%m 10)) :to 3 :by -1)
                (:collect foo))
          (foo i)))))

(define-test hoop.1.63
  (is equal
      '(10 9 8 7 6 5 4 3)
      (macrolet ((%m (z) z))
        (hoop* ((:step i :from 10 :to (expand-in-current-env (%m 3)) :by -1)
                (:collect foo))
          (foo i)))))

(define-test hoop.1.64
  :compile-at :execute
  (is equal
      '(1 2 3 4)
      (macrolet ((%m (z) z))
        (hoop* ((:step i :from (expand-in-current-env (%m 1)) :before 5)
                (:collect foo))
          (foo i)))))

(define-test hoop.1.65
  :compile-at :execute
  (is equal
      '(1 2 3 4)
      (macrolet ((%m (z) z))
        (hoop* ((:step i :from 1 :before (expand-in-current-env (%m 5)))
                (:collect foo))
          (foo i)))))
