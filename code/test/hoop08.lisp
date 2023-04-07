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

(define-test hoop.8.5
  :compile-at :execute
  (is equal
      1
      (hoop* ((:with a := 1))
        (declare (type t a))
        (return a))))

(define-test hoop.8.6
  :compile-at :execute
  (is equal
      2
      (hoop* ((:with a := 2))
        (declare (type fixnum a))
        (return a))))

(define-test hoop.8.7
  :compile-at :execute
  (is equal
      3.0
      (hoop* ((:with a := 3.0))
        (declare (type float a))
        (return a))))

(define-test hoop.8.8
  :compile-at :execute
  (is equal
"abc"
      (hoop* ((:with a := "abc"))
        (declare (type string a))
        (return a))))

(define-test hoop.8.9
  :compile-at :execute
  (is equal
      '(2 1)
      (hoop* ((:with ((a b)) := '(1 2)))
        (return (list b a)))))

(define-test hoop.8.10
  :compile-at :execute
  (is equal
      7
      (hoop* ((:with ((a b)) := '(3 4)))
        (declare (type fixnum a b))
        (return (+ a b)))))

;;; hoop.8.11 thru hoop.8.17 skipped since HOOP doesn't pick default values.

;;; The NIL block of a hoop encloses the entire hoop.

(define-test hoop.8.18
  :compile-at :execute
  (is equal
      t
      (hoop* ((:with nil := (return t)))
        (return nil))))

(define-test hoop.8.19
  :compile-at :execute
  (is equal
      2
      (hoop* ((:with ((nil a)) := '(1 2)))
        (return a))))

(define-test hoop.8.20
  :compile-at :execute
  (is equal
      1
      (hoop* ((:with ((a nil)) := '(1 2)))
        (return a))))

(define-test hoop.8.21
  :compile-at :execute
  (is equal
      '(1 3)
      (hoop* ((:parallel (:with b := 3)
                         (:with ((a nil)) := '(1 2))))
        (return (list a b)))))

(define-test hoop.8.22
  :compile-at :execute
  (is equal
      '(2 3)
      (hoop* ((:parallel (:with b := 3)
                         (:with ((nil a)) := '(1 2))))
        (return (list a b)))))

;;; The NIL block of a hoop encloses the entire hoop.

(define-test hoop.8.23
  :compile-at :execute
  (is equal
      2
      (hoop* ((:parallel (:with a := 1)
                         (:with b := (return 2))))
        (return 3))))

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(define-test hoop.8.24
  :compile-at :execute
  (is equal
      1
      (macrolet ((%m (z) z))
        (hoop* ((:with x := (expand-in-current-env (%m 1))))
          (return x)))))

;;; Test that the variable list may be shorter than values list.

(define-test hoop.8.25
  :compile-at :execute
  (is-values (hoop* ((:with ((a b)) := '(1))
                     (:with ((c d)) := '(2)))
               (return (values a b c d)))
             (equal 1)
             (equal nil)
             (equal 2)
             (equal nil)))

(define-test hoop.8.26
  :compile-at :execute
  (is-values (hoop* ((:with ((a b . rest)) := '(1))
                     (:with ((c d)) := '(2)))
               (return (values a b c d rest)))
             (equal 1)
             (equal nil)
             (equal 2)
             (equal nil)
             (equal nil)))

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
  :compile-at :execute
  (fail-compile (hoop* ((:parallel (:with a := 1)
                                   (:with a := 2)))
                  (return a))
                program-error))

(define-test hoop.8.error.2
  :compile-at :execute
  (fail-compile (hoop* ((:with a := 1)
                        (:with a := 2))
                  (return a))
                program-error))
