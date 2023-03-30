(in-package #:hoop/test)

;;;; Miscellaneous hoop tests

(define-test hoop.17.1
  :compile-at :execute
  (is equal
      336
      (hoop* ((:with x := 0)
              (:prologue (incf x 1)
                         (incf x (+ x x)))
              (:prologue (incf x (+ x x x)))
              (:until t)
              (:epilogue (incf x 100)
                         (incf x (+ x x)))
              (:epilogue (return x))))))

(define-test hoop.17.2
  :compile-at :execute
  (is equal
      336
      (hoop* ((:with x := 0)
              (:until t)
              (:prologue (incf x 1)
                         (incf x (+ x x)))
              (:epilogue (incf x 100)
                         (incf x (+ x x)))
              (:prologue (incf x (+ x x x)))
              (:epilogue (return x))))))

(define-test hoop.17.3
  :compile-at :execute
  (is-values (let ((x 0))
               (hoop* ((:with y := (incf x 1))
                       (:prologue (incf x 2))
                       (:until t)
                       (:epilogue (return (values x y))))))
             (equal 3)
             (equal 1)))

(define-test hoop.17.4
  :compile-at :execute
  (is equal
      'a
      (hoop* ((:epilogue (return 'b)))
        (return 'a))))

(define-test hoop.17.6
  :compile-at :execute
  (is equal
      0
      (let ((x 0))
        (tagbody
           (hoop* ((:epilogue (incf x)))
             (go done))
         done)
        x)))

(define-test hoop.17.7
  :compile-at :execute
  (is equal
      0
      (let ((x 0))
        (catch 'done
          (hoop* ((:epilogue (incf x)))
            (throw 'done nil)))
        x)))

(define-test hoop.17.8
  :compile-at :execute
  (is equal
      'good
      (hoop* ((:each-item x :in '(1 2 3))
              (:collect c)
              (:epilogue (return 'good)))
        (c x))))

(define-test hoop.17.9
  :compile-at :execute
  (is equal
      'good
      (hoop* ((:each-item x :in '(1 2 3))
              (:collect c)
              (:epilogue (return 'good)))
        (c (list x) :append))))

(define-test hoop.17.10
  :compile-at :execute
  (is equal
      'good
      (hoop* ((:each-item x :in '(1 2 3))
              (:collect c)
              (:epilogue (return 'good)))
        (c (list x) :nconc))))

#|(define-test hoop.17.11
(hoop
for x in '(1 2 3)
count (> x 1)
:epilogue (return 'good))
good)

(define-test hoop.17.12
(hoop
for x in '(1 2 3)
sum x
:epilogue (return 'good))
good)

(define-test hoop.17.13
(hoop
for x in '(1 2 3)
maximize x
:epilogue (return 'good))
good)

(define-test hoop.17.14
(hoop
for x in '(1 2 3)
minimize x
:epilogue (return 'good))
good)

;;; iteration clause grouping

(define-test hoop.17.20
(hoop
for i from 1 to 5
for j := 0 then (+ j i)
collect j)
(0 2 5 9 14))

(define-test hoop.17.21
(hoop
for i from 1 to 5
and j := 0 then (+ j i)
collect j)
(0 1 3 6 10))

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(define-test hoop.17.22
(macrolet
((%m (z) z))
(hoop* :with x := 0
:prologue (expand-in-current-env (%m (incf x)))
:until t
:epilogue (expand-in-current-env (%m (return x)))))
1)
|#
