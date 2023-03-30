(in-package #:hoop/test)

;;; Tests of ALWAYS clauses

(define-test hoop.12.1
  :compile-at :execute
  (is equal
      t
      (hoop* ((:each-item i :in '(1 2 3 4))
              (:always (< i 10))))))

(define-test hoop.12.2
  :compile-at :execute
  (is equal
      t
      (hoop* ((:each-item i :in nil)
              (:always nil)))))

(define-test hoop.12.3
  :compile-at :execute
  (false 
   (hoop* ((:each-item i :in '(a))
           (:always nil)))))

(define-test hoop.12.4
  :compile-at :execute
  (is equal
      t
      (hoop* ((:each-item i :in '(1 2 3 4 5 6 7))
              (:always t)
              (:until (> i 5))))))

(define-test hoop.12.5
  :compile-at :execute
  (is equal
      t
      (hoop* ((:each-item i :in '(1 2 3 4 5 6 7))
              (:always (< i 6))
              (:until (>= i 5))))))

(define-test hoop.12.6
  :compile-at :execute
  (is equal
      t
      (hoop* ((:each-item x :in '(a b c d e))
              (:always x)))))

(define-test hoop.12.7
  :compile-at :execute
  (is equal
      t
      (hoop* ((:each-item x :in '(1 2 3 4 5 6))
              (:always (< x 20))
              (:never (> x 10))))))

(define-test hoop.12.8
  :compile-at :execute
  (is-values (hoop* ((:each-item x :in '(1 2 3 4 5 6))
                     (:always (< x 20))
                     (:never (> x 5))))
             (equal t)
             (equal nil)))

(define-test hoop.12.9
  :compile-at :execute
  (false
   (hoop* ((:each-item x :in '(1 2 3 4 5 6))
           (:never (> x 5))
           (:always (< x 20))))))

(define-test hoop.12.10
  :compile-at :execute
  (is equal
      :good
      (hoop* ((:each-item x :in '(1 2 3 4 5))
              (:always (< x 10))
              (:epilogue (return :good))))))

#+(or)(define-test hoop.12.11
  :compile-at :execute
  (false
   (hoop* ((:each-item x :in '(1 2 3 4 5))
           (:always (< x 3))
           (:return :bad)))))

(define-test hoop.12.12
  :compile-at :execute
  (is equal
      t
      (hoop* ((:each-item x :in '(1 2 3 4 5 6))
              (:always t))
        (when (= x 4)
          (hoop-finish)))))

(define-test hoop.12.13
  :compile-at :execute
  (is equal
      t
      (hoop* ((:each-item x :in '(1 2 3 4 5 6))
              (:prologue (hoop-finish))
              (:always nil)))))

;;; Tests of NEVER

(define-test hoop.12.21
  :compile-at :execute
  (is equal
      t
      (hoop* ((:each-item i :in '(1 2 3 4))
              (:never (> i 10))))))

(define-test hoop.12.22
  :compile-at :execute
  (is equal
      t
      (hoop* ((:each-item i :in nil)
              (:never t)))))

(define-test hoop.12.23
  :compile-at :execute
  (false
   (hoop* ((:each-item i :in '(a))
           (:never t)))))

(define-test hoop.12.24
  :compile-at :execute
  (is equal
      t
      (hoop* ((:each-item i :in '(1 2 3 4 5 6 7))
              (:never nil)
              (:until (> i 5))))))

(define-test hoop.12.25
  :compile-at :execute
  (is equal
      t
      (hoop* ((:each-item i :in '(1 2 3 4 5 6 7))
              (:never (>= i 6))
              (:until (>= i 5))))))

(define-test hoop.12.26
  :compile-at :execute
  (is equal
      t
      (hoop* ((:each-item x :in '(a b c d e))
              (:never (not x))))))

(define-test hoop.12.30
  :compile-at :execute
  (is equal
      :good
      (hoop* ((:each-item x :in '(1 2 3 4 5))
              (:never (>= x 10))
              (:epilogue (return :good))))))

#+(or)(define-test hoop.12.31
  :compile-at :execute
  (false
   (hoop* ((:each-item x :in '(1 2 3 4 5))
           (:never (>= x 3))
           (:epilogue (return :bad))))))

(define-test hoop.12.32
  :compile-at :execute
  (is equal
      t
      (hoop* ((:each-item x :in '(1 2 3 4 5 6))
              (:never nil))
        (when (= x 4)
          (hoop-finish)))))

(define-test hoop.12.33
  :compile-at :execute
  (is equal
      t
      (hoop* ((:each-item x :in '(1 2 3 4 5 6))
              (:prologue (hoop-finish))
              (:never t)))))

;;; Tests of THEREIS

(define-test hoop.12.41
  :compile-at :execute
  (is equal
      :good
      (hoop* ((:each-item x :in '(1 2 3 4 5))
              (:thereis (and (<= x 3) :good))))))

(define-test hoop.12.42
  :compile-at :execute
  (is equal
      'a
      (hoop* ((:each-item x :in '(nil nil a nil nil))
              (:thereis x)))))

(define-test hoop.12.43
  :compile-at :execute
  (false
   (hoop* ((:each-item x :in '(1 2 3 4 5))
           (:thereis (eql x 4)))
     (when (eql x 2)
       (hoop-finish)))))

;;; Error cases

#|(define-test hoop.12.error.50
(signals-error
(hoop* ((:each-item i from 1 to 10
collect i
(:always (< i 20))
program-error)
t)

(define-test hoop.12.error.50a
(signals-error
(hoop* ((:each-item i from 1 to 10
(:always (< i 20)
collect i)
program-error)
t)

(define-test hoop.12.error.51
(signals-error
(hoop* ((:each-item i from 1 to 10
collect i
(:never (> i 20))
program-error)
t)

(define-test hoop.12.error.51a
(signals-error
(hoop* ((:each-item i from 1 to 10
(:never (> i 20)
collect i)
program-error)
t)

(define-test hoop.12.error.52
(signals-error
(hoop* ((:each-item i from 1 to 10
collect i
thereis (> i 20))
program-error)
t)

(define-test hoop.12.error.52a
(signals-error
(hoop* ((:each-item i from 1 to 10
thereis (> i 20)
collect i)
program-error)
t)

;;; Non-error cases

(define-test hoop.12.53
(hoop* ((:each-item i from 1 to 10
collect i :into foo
(:always (< i 20))
t)

(define-test hoop.12.53a
(hoop* ((:each-item i from 1 to 10
(:always (< i 20)
collect i :into foo)
t)

(define-test hoop.12.54
(hoop* ((:each-item i from 1 to 10
collect i :into foo
(:never (> i 20))
t)

(define-test hoop.12.54a
(hoop* ((:each-item i from 1 to 10
(:never (> i 20)
collect i :into foo)
t)

(define-test hoop.12.55
(hoop* ((:each-item i from 1 to 10
collect i :into foo
thereis i)
1)

(define-test hoop.12.55a
(hoop* ((:each-item i from 1 to 10
thereis i
collect i :into foo)
1)

;;; Test that explicit calls to macroexpand :in sub((:each-itemms
;;; are done :in the correct environment

(define-test hoop.12.56
(macrolet
((%m (z) z))
(hoop* ((:each-item i :in '(1 2 3 4) (:always (expand-:in-current-env (%m (< i 10)))))
t)

(define-test hoop.12.57
(macrolet
((%m (z) z))
(hoop* ((:each-item i :in '(1 2 3 4) (:always (expand-:in-current-env (%m t))))
t)

(define-test hoop.12.58
(macrolet
((%m (z) z))
(hoop* ((:each-item i :in '(1 2 3 4) (:never (expand-:in-current-env (%m (>= i 10)))))
t)

(define-test hoop.12.59
(macrolet
((%m (z) z))
(hoop* ((:each-item i :in '(1 2 3 4) (:never (expand-:in-current-env (%m t))))
nil)

(define-test hoop.12.60
(macrolet
((%m (z) z))
(hoop* ((:each-item i :in '(1 2 3 4)
thereis (expand-in-current-env (%m (and (>= i 2) (+ i 1))))))
3)


|#
