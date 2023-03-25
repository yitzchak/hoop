(in-package #:hoop/test)

#|
(define-test hoop.14.1
  (hoop for x from 1 to 6
        when (evenp x)
        collect x)
  (2 4 6))

(define-test hoop.14.2
  (hoop for x from 1 to 6
        unless (evenp x)
        collect x)
  (1 3 5))

(define-test hoop.14.3
  (hoop for x from 1 to 10
        when (evenp x)
          collect x into foo
          and count t into bar
        finally (return (values foo bar)))
  (2 4 6 8 10)
  5)

(define-test hoop.14.4
  (hoop for x from 1 to 10
        when (evenp x) collect x end)
  (2 4 6 8 10))

(define-test hoop.14.5
  (hoop for x from 1 to 10
        when (evenp x) collect x into evens
        else collect x into odds
        end
        finally (return (values evens odds)))
  (2 4 6 8 10)
  (1 3 5 7 9))

(define-test hoop.14.6
  (hoop for x from 1 to 10
        unless (oddp x)
          collect x into foo
          and count t into bar
        finally (return (values foo bar)))
  (2 4 6 8 10)
  5)

(define-test hoop.14.7
  (hoop for x from 1 to 10
        unless (oddp x) collect x end)
  (2 4 6 8 10))

(define-test hoop.14.8
  (hoop for x from 1 to 10
        unless (oddp x) collect x into evens
        else collect x into odds
        end
        finally (return (values evens odds)))
  (2 4 6 8 10)
  (1 3 5 7 9))

(define-test hoop.14.9
  (hoop for x from 1 to 6
        if (evenp x)
        collect x)
  (2 4 6))

(define-test hoop.14.10
  (hoop for x from 1 to 10
        if (evenp x)
          collect x into foo
          and count t into bar
        finally (return (values foo bar)))
  (2 4 6 8 10)
  5)

(define-test hoop.14.11
  (hoop for x from 1 to 10
        if (evenp x) collect x end)
  (2 4 6 8 10))

(define-test hoop.14.12
  (hoop for x from 1 to 10
        if (evenp x) collect x into evens
        else collect x into odds
        end
        finally (return (values evens odds)))
  (2 4 6 8 10)
  (1 3 5 7 9))

;;; Test that else associates with the nearest conditional unclosed
;;; by end

(define-test hoop.14.13
  (hoop for i from 1 to 20
        if (evenp i)
          if (= (mod i 3) 0)
            collect i into list1
            else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (2 4 8 10 14 16 20))

(define-test hoop.14.14
  (hoop for i from 1 to 20
        when (evenp i)
          if (= (mod i 3) 0)
            collect i into list1
            else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (2 4 8 10 14 16 20))

(define-test hoop.14.15
  (hoop for i from 1 to 20
        if (evenp i)
          when (= (mod i 3) 0)
            collect i into list1
            else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (2 4 8 10 14 16 20))

(define-test hoop.14.16
  (hoop for i from 1 to 20
        if (evenp i)
          if (= (mod i 3) 0)
            collect i into list1
          end
          else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (1 3 5 7 9 11 13 15 17 19))

(define-test hoop.14.17
  (hoop for i from 1 to 20
        when (evenp i)
          if (= (mod i 3) 0)
            collect i into list1
          end
          else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (1 3 5 7 9 11 13 15 17 19))

(define-test hoop.14.18
  (hoop for i from 1 to 20
        if (evenp i)
          when (= (mod i 3) 0)
            collect i into list1
          end
          else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (1 3 5 7 9 11 13 15 17 19))

(define-test hoop.14.19
  (hoop for i from 1 to 20
        when (evenp i)
          when (= (mod i 3) 0)
            collect i into list1
          end
          else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (1 3 5 7 9 11 13 15 17 19))

(define-test hoop.14.20
  (hoop for i from 1 to 20
        unless (oddp i)
          if (= (mod i 3) 0)
            collect i into list1
          end
          else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (1 3 5 7 9 11 13 15 17 19))

(define-test hoop.14.21
  (hoop for i from 1 to 20
        if (evenp i)
          unless (/= (mod i 3) 0)
            collect i into list1
          end
          else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (1 3 5 7 9 11 13 15 17 19))

(define-test hoop.14.22
  (hoop for i from 1 to 20
        unless (oddp i)
          unless (/= (mod i 3) 0)
            collect i into list1
          end
          else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (1 3 5 7 9 11 13 15 17 19))

;;; More tests conditionals

(define-test hoop.14.23
  (hoop for i from 1 to 20
        if (evenp i)
          collect i into list1
        else if (= (mod i 3) 0)
          collect i into list2
        else collect i into list3
        finally (return (values list1 list2 list3)))
  (2 4 6 8 10 12 14 16 18 20)
  (3 9 15)
  (1 5 7 11 13 17 19))

;;; Tests of 'IT'

(define-test hoop.14.24
  (hoop for x in '((a) nil (b) (c) (nil) (d))
        when (car x) collect it)
  (a b c d))

(define-test hoop.14.25
  (hoop for x in '((a) nil (b) (c) (nil) (d))
        if (car x) collect it)
  (a b c d))

(define-test hoop.14.26
  (hoop for x in '(nil (a) nil (b) (c) (nil) (d))
        when (car x) return it)
  a)

(define-test hoop.14.27
  (hoop for x in '(nil (a) nil (b) (c) (nil) (d))
        if (car x) return it)
  a)

(define-test hoop.14.28
  (hoop for x in '((a) nil (b) (c) (nil) (d))
        when (car x) collect it and collect 'foo)
  (a foo b foo c foo d foo))

(define-test hoop.14.29
  (let ((it 'z))
    (hoop for x in '(a b c d)
          when x collect it and collect it))
  (a z b z c z d z))

(define-test hoop.14.30
  (let ((it 'z))
    (hoop for x in '(a b c d)
          if x collect it end
          collect it))
  (a z b z c z d z))

(define-test hoop.14.31
  (hoop for it on '(a b c d)
        when (car it) collect it)
  (a b c d))

(define-test hoop.14.32
  (hoop for x in '(a b nil c d nil e)
        when x collecting it)
  (a b c d e))

(define-test hoop.14.33
  (hoop for x in '(a b nil c d nil e)
        when x append (list x))
  (a b c d e))

(define-test hoop.14.34
  (hoop for x in '(a b nil c d nil e)
        when x appending (list x))
  (a b c d e))

(define-test hoop.14.35
  (hoop for x in '(a b nil c d nil e)
        when x nconc (list x))
  (a b c d e))

(define-test hoop.14.36
  (hoop for x in '(a b nil c d nil e)
        when x nconcing (list x))
  (a b c d e))

(define-test hoop.14.37
  (hoop for it on '(a b c d)
        when (car it) collect it into foo
        finally (return foo))
  (a b c d))

(define-test hoop.14.38
  (hoop for x in '(1 2 nil 3 4 nil 5 nil)
        when x count it)
  5)

(define-test hoop.14.39
  (hoop for x in '(1 2 nil 3 4 nil 5 nil)
        when x counting it)
  5)

(define-test hoop.14.40
  (hoop for x in '(1 2 nil 3 4 nil 6 nil)
        when x maximize it)
  6)

(define-test hoop.14.41
  (hoop for x in '(1 2 nil 3 4 nil 6 nil)
        when x maximizing it)
  6)

(define-test hoop.14.42
  (hoop for x in '(1 2 nil 3 4 nil 6 nil)
        when x minimize it)
  1)

(define-test hoop.14.43
  (hoop for x in '(1 2 nil 3 4 nil 6 nil)
        when x minimizing it)
  1)

(define-test hoop.14.44
  (hoop for x in '(1 2 nil 3 4 nil 6 nil)
        when x sum it)
  16)

(define-test hoop.14.45
  (hoop for x in '(1 2 nil 3 4 nil 6 nil)
        when x summing it)
  16)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(define-test hoop.14.46
  (macrolet
   ((%m (z) z))
   (hoop for x from 1 to 6
         when (expand-in-current-env (%m (evenp x)))
         collect x))
  (2 4 6))

(define-test hoop.14.47
  (macrolet
   ((%m (z) z))
   (hoop for x from 1 to 6
         unless (expand-in-current-env (%m (evenp x)))
         collect x))
  (1 3 5))

(define-test hoop.14.48
  (macrolet
   ((%m (z) z))
   (hoop for x from 1 to 6
         when (expand-in-current-env (%m t))
         sum x))
  21)

(define-test hoop.14.49
  (macrolet
   ((%m (z) z))
   (hoop for x from 1 to 10
         if  (expand-in-current-env (%m (evenp x)))
         collect x end))
  (2 4 6 8 10))
|#
