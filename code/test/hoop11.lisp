(in-package #:hoop/test)

(define-test hoop.11.1
  :compile-at :execute
  (is-values (let ((z 0))
               (values (hoop* ((:repeat 10))
                         (incf z))
                       z))
             (equal nil)
             (equal 10)))

(define-test hoop.11.2
  :compile-at :execute
  (is equal
      '(a a a a a a a a a a)
      (hoop* ((:repeat 10)
              (:collect c))
        (c 'a))))

(define-test hoop.11.3
  :compile-at :execute
  (is equal
      0
      (let ((z 0))
        (hoop* ((:repeat 0))
          (incf z))
        z)))

(define-test hoop.11.4
  :compile-at :execute
  (is equal
      0
      (let ((z 0))
        (hoop* ((:repeat -1))
          (incf z))
        z)))

(define-test hoop.11.5
  :compile-at :execute
  (is equal
      0
      (let ((z 0))
        (hoop* ((:repeat -1.5))
          (incf z))
        z)))

(define-test hoop.11.6
  :compile-at :execute
  (is equal
      0
      (let ((z 0))
        (hoop* ((:repeat -1000000000000))
          (incf z))
        z)))

(define-test hoop.11.7
  :compile-at :execute
  (is equal
      1
      (let ((z 0))
        (hoop* ((:repeat 10))
          (incf z)
          (hoop-finish))
        z)))

;;; This test is wrong because ((:REPEAT is a main clause whereas FOR is
;;; a variable clause, and no main clause can precede a variable
;;; clause.

(define-test hoop.11.8
  :compile-at :execute
  (is equal
      '(a b c)
      (hoop* ((:repeat 3)
              (:each-item i :in '(a b c d e))
              (:collect c))
        (c i))))

;;; Enough implementors have complained about this test that
;;; I'm removing it.  The standard is self-contradictory
;;; on whether ((:REPEAT can occur later in a HOOP form.

(define-test hoop.11.9
  :compile-at :execute
  (is equal
      '(a b c)
      (hoop* ((:each-item i :in '(a b c d e))
              (:collect c)
              (:repeat 3))
        (c i))))

;;; Tests of WHILE

(define-test hoop.11.10
  :compile-at :execute
  (is equal
      '(1 2 3 4 5 6 7 8 9 10)
      (hoop* ((:with i := 0)
              (:while (< i 10))
              (:collect c))
        (c (incf i)))))

(define-test hoop.11.11
  :compile-at :execute
  (is equal
      'good
      (hoop* ((:with i := 0)
              (:while (if (< i 10) t (return 'good)))
              (:collect c))
        (c (incf i)))))
#|
(define-test hoop.11.12
(hoop* ((:with i = 0
(:while (< i 10) (:collect (incf i)
(:while (< i 10) (:collect (incf i)
(:while (< i 10) (:collect (incf i))
(1 2 3 4 5 6 7 8 9 10))

(define-test hoop.11.13
(hoop* ((:with i := 0)
(:while (< i 10))
(:collect (incf i)
finally (return 'done))
done)

(define-test hoop.11.14
(hoop* for i in '(a b c)
(:while nil
(:collect i)
nil)

(define-test hoop.11.15
(hoop* for i in '(a b c)
(:collect i
(:while nil)
(a))

(define-test hoop.11.16
(hoop* for i in '(a b c)
(:while t
(:collect i)
(a b c))

(define-test hoop.11.17
(hoop* for i in '(a b c)
(:collect i
(:while t)
(a b c))

(define-test hoop.11.18
(hoop* for i from 1 to 10
(:while (< i 6)
finally (return i))
6)

;;; Tests of UNTIL

(define-test hoop.11.20
(hoop* ((:with i = 0 until (>= i 10) (:collect (incf i))
(1 2 3 4 5 6 7 8 9 10))

(define-test hoop.11.21
(hoop* ((:with i = 0 (:while (if (< i 10) t (return 'good))
(:collect (incf i))
good)

(define-test hoop.11.22
(hoop* ((:with i = 0
until (>= i 10) (:collect (incf i)
until (>= i 10) (:collect (incf i)
until (>= i 10) (:collect (incf i))
(1 2 3 4 5 6 7 8 9 10))

(define-test hoop.11.23
(hoop* ((:with i = 0 until (>= i 10) (:collect (incf i)
finally (return 'done))
done)

(define-test hoop.11.24
(hoop* for i in '(a b c)
until t
(:collect i)
nil)

(define-test hoop.11.25
(hoop* for i in '(a b c)
(:collect i
until t)
(a))

(define-test hoop.11.26
(hoop* for i in '(a b c)
until nil
(:collect i)
(a b c))

(define-test hoop.11.27
(hoop* for i in '(a b c)
(:collect i
until nil)
(a b c))

(define-test hoop.11.28
(hoop* for i from 1 to 10
until (>= i 6)
finally (return i))
6)

;;; More tests of a bug that showed up in c.l.l

(define-test hoop.11.29
(hoop* for i in '(4 8 9 A 13)
when (eq i 'a) return :good
(:while (< i 12) (:collect i)
:good)

(define-test hoop.11.30
(hoop* for i in '(4 8 9 A 13)
unless (numberp i) return :good
(:while (< i 12) (:collect i)
:good)

(define-test hoop.11.31
(hoop* for i in '(4 8 9 A 13)
when (eq i 'a) return :good
until (> i 12) (:collect i)
:good)

(define-test hoop.11.32
(hoop* for i in '(4 8 9 A 13)
unless (numberp i) return :good
until (> i 12) (:collect i)
:good)

(define-test hoop.11.33
(hoop* for i in '(4 8 9 A 13)
if (not (numberp i)) return :good end
(:while (< i 12) (:collect i)
:good)

(define-test hoop.11.34
(hoop* for i in '(4 8 9 A 13)
if (not (numberp i)) return :good end
until (> i 12) (:collect i)
:good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(define-test hoop.11.35
(macrolet
((%m (z) z))
(hoop* ((:repeat (expand-in-current-env (%m 5)) (:collect 'x))
(x x x x x))
|#
