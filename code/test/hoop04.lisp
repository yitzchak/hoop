(in-package #:hoop/test)

(define-test hoop.4.1
  :compile-at :execute
  (is equal
      '(1 2 3 4 5)
      (hoop* ((:generate x :using 1 :then (1+ x))
              (:until (> x 5))
              (:collect c))
        (c x))))

(define-test hoop.4.2
  :compile-at :execute
  (is equal
      '(2 3 4 5 6 7 8 9 10 11)
      (hoop* ((:step i :from 1 :to 10)
              (:generate j :using (1+ i))
              (:collect c))
        (c j))))

(define-test loop.4.3
  :compile-at :execute
  (is equal
      '(2 3 4 5 6 7 8 9 10 11)
      (hoop* ((:step i :from 1 :to 10)
              (:generate j :using (1+ i))
              (:collect c))
        (declare (type integer j))
        (c j))))

(define-test hoop.4.4
  :compile-at :execute
  (is equal
      '(a b c d e)
      (hoop* ((:each-sublist e :in '(a b c d e))
              (:generate ((x . y)) :using e)
              (:collect c))
        (c x))))

(define-test hoop.4.5
  :compile-at :execute
  (is equal
      '(a b c d e)
      (hoop* ((:generate ((x . y)) :using '(a b c d e) :then y)
              (:while x)
              (:collect c))
        (c x))))

;;; Error cases

(define-test hoop.4.6
  :compile-at :execute
  (fail-compile (hoop* ((:generate (x . x) :using '(nil nil nil))
                        (:until x)
                        (:count c))
                  (c t))
                program-error))

(define-test hoop.4.7
  :compile-at :execute
  (fail (macroexpand '(hoop* ((:generate (x . x) :using '(nil nil nil))
                              (:until x)
                              (:count c))
                       (c t)))
        program-error))

(define-test hoop.4.8
  :compile-at :execute
  (fail (macroexpand '(hoop* ((:generate c :using '(nil nil nil))
                              (:generate x :using 1)
                              (:count c)
                              (:until t))
                       (c t)))
        program-error))

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

#|(define-test hoop.4.9
(macrolet
((%m (z) z))
(hoop
for x = (expand-in-current-env (%m 1)) then (1+ x)
until (> x 5)
collect x))
(1 2 3 4 5))

(define-test hoop.4.10
(macrolet
((%m (z) z))
(hoop
for x = 1 then (expand-in-current-env (%m (1+ x)))
until (> x 5)
collect x))
(1 2 3 4 5))

(define-test hoop.4.11
(macrolet
((%m (z) z))
(hoop
for x = 1 then (1+ x)
until (expand-in-current-env (%m (> x 5)))
collect x))
(1 2 3 4 5))

(define-test hoop.4.12
(macrolet
((%m (z) z))
(hoop
for x = 1 then (1+ x)
while (expand-in-current-env (%m (<= x 5)))
collect x))
(1 2 3 4 5))

(define-test hoop.4.13
(macrolet
((%m (z) z))
(hoop
for x = 1 then (1+ x)
until (> x 5)
collect (expand-in-current-env (%m x))))
(1 2 3 4 5))
|#
