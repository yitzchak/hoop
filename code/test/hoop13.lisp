(in-package #:hoop/test)

(define-test hoop.13.1
  :compile-at :execute
  (is equal
      10
      (hoop* ()
        (return 10))))

(define-test hoop.13.3
  :compile-at :execute
  (is equal
      56
      (hoop* ((:step i :from 0 :before 100 :by 7))
        (when (> i 50)
          (return i)))))

(define-test hoop.13.4
  :compile-at :execute
  (is equal
      10
      (let ((x 0))
        (hoop* ()
          (incf x)
          (when (= x 10)
            (return x))))))

(define-test hoop.13.6
  :compile-at :execute
  (is-values
   (hoop* ()
     (return (values)))))

(define-test hoop.13.7
  :compile-at :execute
  (is-values
   (hoop* ()
     (return (values 1 2)))
   (equal 1)
   (equal 2)))

#|(define-test hoop.13.8
(let* ((limit (min 1000 (1- (min call-arguments-limit
multiple-values-limit))))
(vals (make-list limit :initial-element :a))
(vals2 (multiple-value-list (eval `(hoop* return (values ,@vals))))))
(equalt vals vals2))
t)

(define-test hoop.13.9
(hoop* named foo return 'a)
a)

(define-test hoop.13.10
(block nil
(return (hoop* named foo return :good))
:bad)
:good)

(define-test hoop.13.11
(block nil
(hoop* named foo do (return :good))
:bad)
:good)

(define-test hoop.13.12
(hoop* named foo with a = (return-from foo :good) return :bad)
:good)

(define-test hoop.13.13
(hoop* named foo
with b = 1
and a = (return-from foo :good) return :bad)
:good)

(define-test hoop.13.14
(hoop* named foo
for a = (return-from foo :good) return :bad)
:good)

(define-test hoop.13.15
(hoop* named foo for a in (return-from foo :good))
:good)

(define-test hoop.13.16
(hoop* named foo for a from (return-from foo :good) return :bad)
:good)

(define-test hoop.13.17
(hoop* named foo for a on (return-from foo :good) return :bad)
:good)

(define-test hoop.13.18
(hoop* named foo for a across (return-from foo :good) return :bad)
:good)

(define-test hoop.13.19
(hoop* named foo for a being the hash-keys of (return-from foo :good)
return :bad)
:good)

(define-test hoop.13.20
(hoop* named foo for a being the symbols of (return-from foo :good)
return :bad)
:good)

(define-test hoop.13.21
(hoop* named foo repeat (return-from foo :good) return :bad)
:good)

(define-test hoop.13.22
(hoop* named foo for i from 0 to (return-from foo :good) return :bad)
:good)

(define-test hoop.13.23
(hoop* named foo for i from 0 to 10 by (return-from foo :good) return :bad)
:good)

(define-test hoop.13.24
(hoop* named foo for i from 10 downto (return-from foo :good) return :bad)
:good)

(define-test hoop.13.25
(hoop* named foo for i from 10 above (return-from foo :good) return :bad)
:good)

(define-test hoop.13.26
(hoop* named foo for i from 10 below (return-from foo :good) return :bad)
:good)

(define-test hoop.13.27
(hoop* named foo for i in '(a b c) by (return-from foo :good) return :bad)
:good)

(define-test hoop.13.28
(hoop* named foo for i on '(a b c) by (return-from foo :good) return :bad)
:good)

(define-test hoop.13.29
(hoop* named foo for i = 1 then (return-from foo :good))
:good)

(define-test hoop.13.30
(hoop* named foo for x in '(a b c) collect (return-from foo :good))
:good)

(define-test hoop.13.31
(hoop* named foo for x in '(a b c) append (return-from foo :good))
:good)

(define-test hoop.13.32
(hoop* named foo for x in '(a b c) nconc (return-from foo :good))
:good)

(define-test hoop.13.33
(hoop* named foo for x in '(a b c) count (return-from foo :good))
:good)

(define-test hoop.13.34
(hoop* named foo for x in '(a b c) sum (return-from foo :good))
:good)

(define-test hoop.13.35
(hoop* named foo for x in '(a b c) maximize (return-from foo :good))
:good)

(define-test hoop.13.36
(hoop* named foo for x in '(a b c) minimize (return-from foo :good))
:good)

(define-test hoop.13.37
(hoop* named foo for x in '(a b c) thereis (return-from foo :good))
:good)

(define-test hoop.13.38
(hoop* named foo for x in '(a b c) always (return-from foo :good))
:good)

(define-test hoop.13.39
(hoop* named foo for x in '(a b c) never (return-from foo :good))
:good)

(define-test hoop.13.40
(hoop* named foo for x in '(a b c) until (return-from foo :good))
:good)

(define-test hoop.13.41
(hoop* named foo for x in '(a b c) while (return-from foo :good))
:good)

(define-test hoop.13.42
(hoop* named foo for x in '(a b c) when (return-from foo :good) return :bad)
:good)

(define-test hoop.13.43
(hoop* named foo for x in '(a b c) unless (return-from foo :good) return :bad)
:good)

(define-test hoop.13.44
(hoop* named foo for x in '(a b c) if (return-from foo :good) return :bad)
:good)

(define-test hoop.13.45
(hoop* named foo for x in '(a b c) return (return-from foo :good))
:good)

(define-test hoop.13.46
(hoop* named foo initially (return-from foo :good) return :bad)
:good)

(define-test hoop.13.47
(hoop* named foo do (hoop-finish) finally (return-from foo :good))
:good)


(define-test hoop.13.52
(block nil
(hoop* named foo with a = (return :good) return :bad)
:bad)
:good)

(define-test hoop.13.53
(block nil
(hoop* named foo
with b = 1
and a = (return :good) return :bad)
:bad)
:good)

(define-test hoop.13.54
(block nil
(hoop* named foo
for a = (return :good) return :bad)
:bad)
:good)

(define-test hoop.13.55
(block nil
(hoop* named foo for a in (return :good))
:bad)
:good)

(define-test hoop.13.56
(block nil
(hoop* named foo for a from (return :good) return :bad)
:bad)
:good)

(define-test hoop.13.57
(block nil
(hoop* named foo for a on (return :good) return :bad)
:bad)
:good)

(define-test hoop.13.58
(block nil
(hoop* named foo for a across (return :good) return :bad)
:bad)
:good)

(define-test hoop.13.59
(block nil
(hoop* named foo for a being the hash-keys of (return :good)
return :bad)
:bad)
:good)

(define-test hoop.13.60
(block nil
(hoop* named foo for a being the symbols of (return :good)
return :bad)
:bad)
:good)

(define-test hoop.13.61
(block nil
(hoop* named foo repeat (return :good) return :bad)
:bad)
:good)

(define-test hoop.13.62
(block nil
(hoop* named foo for i from 0 to (return :good) return :bad)
:bad)
:good)

(define-test hoop.13.63
(block nil
(hoop* named foo for i from 0 to 10 by (return :good) return :bad)
:bad)
:good)

(define-test hoop.13.64
(block nil
(hoop* named foo for i from 10 downto (return :good) return :bad)
:bad)
:good)

(define-test hoop.13.65
(block nil
(hoop* named foo for i from 10 above (return :good) return :bad)
:bad)
:good)

(define-test hoop.13.66
(block nil
(hoop* named foo for i from 10 below (return :good) return :bad)
:bad)
:good)

(define-test hoop.13.67
(block nil
(hoop* named foo for i in '(a b c) by (return :good) return :bad)
:bad)
:good)

(define-test hoop.13.68
(block nil
(hoop* named foo for i on '(a b c) by (return :good) return :bad)
:bad)
:good)

(define-test hoop.13.69
(block nil
(hoop* named foo for i = 1 then (return :good))
:bad)
:good)

(define-test hoop.13.70
(block nil
(hoop* named foo for x in '(a b c) collect (return :good))
:bad)
:good)

(define-test hoop.13.71
(block nil
(hoop* named foo for x in '(a b c) append (return :good))
:bad)
:good)

(define-test hoop.13.72
(block nil
(hoop* named foo for x in '(a b c) nconc (return :good))
:bad)
:good)

(define-test hoop.13.73
(block nil
(hoop* named foo for x in '(a b c) count (return :good))
:bad)
:good)

(define-test hoop.13.74
(block nil
(hoop* named foo for x in '(a b c) sum (return :good))
:bad)
:good)

(define-test hoop.13.75
(block nil
(hoop* named foo for x in '(a b c) maximize (return :good))
:bad)
:good)

(define-test hoop.13.76
(block nil
(hoop* named foo for x in '(a b c) minimize (return :good))
:bad)
:good)

(define-test hoop.13.77
(block nil
(hoop* named foo for x in '(a b c) thereis (return :good))
:bad)
:good)

(define-test hoop.13.78
(block nil
(hoop* named foo for x in '(a b c) always (return :good))
:bad)
:good)

(define-test hoop.13.79
(block nil
(hoop* named foo for x in '(a b c) never (return :good))
:bad)
:good)

(define-test hoop.13.80
(block nil
(hoop* named foo for x in '(a b c) until (return :good))
:bad)
:good)

(define-test hoop.13.81
(block nil
(hoop* named foo for x in '(a b c) while (return :good))
:bad)
:good)

(define-test hoop.13.82
(block nil
(hoop* named foo for x in '(a b c) when (return :good) return :bad)
:bad)
:good)

(define-test hoop.13.83
(block nil
(hoop* named foo for x in '(a b c) unless (return :good) return :bad)
:bad)
:good)

(define-test hoop.13.84
(block nil
(hoop* named foo for x in '(a b c) if (return :good) return :bad)
:bad)
:good)

(define-test hoop.13.85
(block nil
(hoop* named foo for x in '(a b c) return (return :good))
:bad)
:good)

(define-test hoop.13.86
(block nil
(hoop* named foo initially (return :good) return :bad)
:bad)
:good)

(define-test hoop.13.87
(block nil
(hoop* named foo do (hoop-finish) finally (return :good))
:bad)
:good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(define-test hoop.13.88
(macrolet
((%m (z) z))
(hoop* do (expand-in-current-env (%m (return 10)))))
10)

(define-test hoop.13.89
(macrolet
((%m (z) z))
(hoop* for i from 0 below 100 by 7
when (> i 50) return (expand-in-current-env (%m i))))
56)

(define-test hoop.13.90
(macrolet
((%m (z) z))
(hoop* return (expand-in-current-env (%m 'a))))
a)
|#
