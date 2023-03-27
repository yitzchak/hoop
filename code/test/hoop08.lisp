(in-package #:hoop/test)

(define-test hoop.8.1
  :compile-at :execute
  (is equal
      1
      (hoop* ((:with x := 1))
        (return x))))

(define-test hoop.8.2
  :compile-at :execute
  (is equal
      '(1 2)
      (hoop* ((:with x := 1)
              (:with y := (1+ x)))
        (return (list x y)))))

(define-test hoop.8.3
  :compile-at :execute
  (is equal
      '(2 3)
      (let ((y 2))
        (hoop* ((:with x := y)
                (:with y := (1+ x)))
          (return (list x y))))))

(define-test hoop.8.4
  :compile-at :execute
  (is equal
      '(1 (nil) (nil))
      (let (a b)
        (hoop* ((:parallel (:with a := 1)
                           (:with b := (list a))
                           (:with c := (list b))))
          (return (list a b c))))))

;;; type specs

#|(define-test hoop.8.5
(hoop* with a t = 1 return a)
1)

(define-test hoop.8.6
(hoop* with a fixnum = 2 return a)
2)

(define-test hoop.8.7
(hoop* with a float = 3.0 return a)
3.0)

(define-test hoop.8.8
(hoop* with a of-type string = "abc" return a)
"abc")

(define-test hoop.8.9
(hoop* with (a b) = '(1 2) return (list b a))
(2 1))

(define-test hoop.8.10
(hoop* with (a b) of-type (fixnum fixnum) = '(3 4) return (+ a b))
7)

(define-test hoop.8.11
(hoop* with a of-type fixnum return a)
0)

(define-test hoop.8.12
(hoop* with a of-type float return a)
0.0)

(define-test hoop.8.13
(hoop* with a of-type t return a)
nil)

(define-test hoop.8.14
(hoop* with a t return a)
nil)

(define-test hoop.8.15
(hoop* with a t and b t return (list a b))
(nil nil))

(define-test hoop.8.16
(hoop* with (a b c) of-type (fixnum float t) return (list a b c))
(0 0.0 nil))

(define-test hoop.8.17
(hoop* with nil = nil return nil)
nil)

;;; The NIL block of a hoop encloses the entire hoop.

(define-test hoop.8.18
(hoop* with nil = (return t) return nil)
t)

(define-test hoop.8.19
(hoop* with (nil a) = '(1 2) return a)
2)

(define-test hoop.8.20
(hoop* with (a nil) = '(1 2) return a)
1)

(define-test hoop.8.21
(hoop* with b = 3
and (a nil) = '(1 2) return (list a b))
(1 3))

(define-test hoop.8.22
(hoop* with b = 3
and (nil a) = '(1 2) return (list a b))
(2 3))

;;; The NIL block of a hoop encloses the entire hoop.

(define-test hoop.8.23
(hoop
with a = 1
and  b = (return 2)
return 3)
2)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(define-test hoop.8.24
(macrolet
((%m (z) z))
(hoop* with x = (expand-in-current-env (%m 1)) do (return x)))
1)

;;; Test that the variable list may be shorter than values list.

(define-test hoop.8.25
(hoop* with (a b) = '(1)
for (c d) = '(2)
do (return (values a b c d)))
1 nil 2 nil)

(define-test hoop.8.26
(hoop* with (a b . rest) = '(1)
for (c d) = '(2)
do (return (values a b c d rest)))
1 nil 2 nil nil)

;;; Error cases

;;; The spec says (in section 6.1.1.7) that:
;;; "An error of type program-error is signaled (at macro expansion time)
;;;  if the same variable is bound twice in any variable-binding clause
;;;  of a single hoop expression. Such variables include local variables,
;;;  iteration control variables, and variables found by destructuring."
;;;
;;; This is somewhat ambiguous.  Test hoop.8.error.1 binds A twice in
;;; the same clause, but hoop.8.error.2 binds A in two different clauses.
;;; I am interpreting the spec as ruling out the latter as well.

(define-test hoop.8.error.1
(signals-error
(hoop* with a = 1
and  a = 2 return a)
program-error)
t)

(define-test hoop.8.error.2
(signals-error
(hoop* with a = 1
with a = 2 return a)
program-error)
t)
|#
