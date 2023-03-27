(in-package #:hoop/test)

(define-test hoop.5.1
  :compile-at :execute
  (is equal
      '(#\a #\b #\c #\d)
      (let ((x "abcd"))
        (hoop* ((:each-elt e :in x)
                (:collect c))
          (c e)))))

(define-test hoop.5.2
  :compile-at :execute
  (is equal
      '(#\a #\b #\c #\d)
      (let ((x "abcd"))
        (hoop* ((:each-elt e :in (the string x))
                (:collect c))
          (c e)))))

(define-test hoop.5.3
  :compile-at :execute
  (is equal
      '(#\a #\b #\c #\d)
      (let ((x "abcd"))
        (hoop* ((:each-elt e :in (the simple-string x))
                (:collect c))
          (c e)))))

(define-test hoop.5.1
  :compile-at :execute
  (is equal
      '(#\a #\b #\c #\d)
      (hoop* ((:each-elt e :in "abcd")
              (:collect c))
        (c e))))

(define-test hoop.5.5
  :compile-at :execute
  (is equal
      '(#\a #\b #\c)
      (hoop* ((:each-elt e :in "abcd" :end 3)
              (:collect c))
        (c e))))

#|

(define-test hoop.5.7
(let ((x (make-array '(4) :initial-contents "abcd" :element-type 'base-char)))
(hoop* ((:each-elt e :in (the base-string x) collect e))
(#\a #\b #\c #\d))

(define-test hoop.5.8
(let ((x "abcd")) (hoop* ((:each-elt e of-type character :in x collect e))
(#\a #\b #\c #\d))

(define-test hoop.5.10
(let ((x #*00010110))
(hoop* ((:each-elt e :in x collect e))
(0 0 0 1 0 1 1 0))

(define-test hoop.5.11
(let ((x #*00010110))
(hoop* ((:each-elt e :in (the bit-vector x) collect e))
(0 0 0 1 0 1 1 0))

(define-test hoop.5.12
(let ((x #*00010110))
(hoop* ((:each-elt e :in (the simple-bit-vector x) collect e))
(0 0 0 1 0 1 1 0))

(define-test hoop.5.13
(let ((x #*00010110))
(hoop* ((:each-elt e of-type bit :in (the simple-bit-vector x) collect e))
(0 0 0 1 0 1 1 0))

(define-test hoop.5.14
(let ((x #*00010110))
(hoop* ((:each-elt e of-type bit :in x
((:each-elt i from 1 to 4 collect e))
(0 0 0 1))


(define-test hoop.5.20
(let ((x (vector 'a 'b 'c 'd)))
(hoop* ((:each-elt e :in x collect e))
(a b c d))

(define-test hoop.5.21
(let ((x (vector 'a 'b 'c 'd)))
(hoop* ((:each-elt e :in (the vector x) collect e))
(a b c d))

(define-test hoop.5.22
(let ((x (vector 'a 'b 'c 'd)))
(hoop* ((:each-elt e :in (the simple-vector x) collect e))
(a b c d))

(define-test hoop.5.23
(let ((x (vector '(a) '(b) '(c) '(d))))
(hoop* ((:each-elt (e) :in x collect e))
(a b c d))


(define-test hoop.5.30
(let ((x (make-array '(5) :initial-contents '(a b c d e)
:adjustable t)))
(hoop* ((:each-elt e :in x collect e))
(a b c d e))

(define-test hoop.5.32
(let* ((x (make-array '(5) :initial-contents '(a b c d e)))
(y (make-array '(3) :displaced-to x
:displaced-index-offset 1)))
(hoop* ((:each-elt e :in y collect e))
(b c d))

;;; tests of 'as' ((:each-eltm

(define-test hoop.5.33
(hoop* as e :in "abc" collect e)
(#\a #\b #\c))

(define-test hoop.5.34
(hoop* as e of-type character :in "abc" collect e)
(#\a #\b #\c))

(define-test hoop.5.35
(hoop* as e of-type integer :in (the simple-vector (coerce '(1 2 3) 'simple-vector))
sum e)
6)

;;; Hoop :in displaced vectors

(define-test hoop.5.36
(let* ((a (make-array '(10) :initial-contents '(a b c d e f g h i j)))
(da (make-array '(5) :displaced-to a
:displaced-index-offset 2)))
(hoop* ((:each-elt e :in da collect e))
(c d e f g))

(define-test hoop.5.37
(let* ((a (make-array '(10) :element-type 'base-char
:initial-contents "abcdefghij"))
(da (make-array '(5) :element-type 'base-char
:displaced-to a
:displaced-index-offset 2)))
(hoop* ((:each-elt e :in da collect e))
(#\c #\d #\e #\f #\g))

(define-test hoop.5.38
(let* ((a (make-array '(10) :element-type 'bit
:initial-contents '(0 1 1 0 0 1 0 1 1 1)))
(da (make-array '(5) :element-type 'bit
:displaced-to a
:displaced-index-offset 2)))
(hoop* ((:each-elt e :in da collect e))
(1 0 0 1 0))

(define-test hoop.5.39
(let ((v (make-array '(10) :initial-contents '(1 2 3 4 5 6 7 8 9 10)
:fill-pointer 6)))
(hoop* ((:each-elt x :in v collect x))
(1 2 3 4 5 6))

(define-test hoop.5.40
(hoop* ((:each-elt i from 1 to 40
((:each-elt type = `(unsigned-byte ,i)
((:each-elt v = (make-array '(10) :initial-contents '(0 0 1 1 0 1 1 1 0 0)
:element-type type)
((:each-elt r = (hoop* ((:each-elt x :in v collect x)
unless (equal r '(0 0 1 1 0 1 1 1 0 0))
collect (list i r))
nil)

(define-test hoop.5.41
(hoop* ((:each-elt i from 1 to 40
((:each-elt type = `(signed-byte ,i)
((:each-elt v = (make-array '(10) :initial-contents '(0 0 -1 -1 0 -1 -1 -1 0 0)
:element-type type)
((:each-elt r = (hoop* ((:each-elt x :in v collect x)
unless (equal r '(0 0 -1 -1 0 -1 -1 -1 0 0))
collect (list i r))
nil)

(define-test hoop.5.42
(let ((vals '(0 0 1 1 0 1 1 1 0 0)))
(hoop* ((:each-elt type in '(short-float single-float double-float long-float)
((:each-elt fvals = (hoop* ((:each-elt v in vals collect (coerce v type))
((:each-elt v = (make-array '(10) :initial-contents fvals :element-type type)
((:each-elt r = (hoop* ((:each-elt x :in v collect x)
unless (equal r fvals)
collect (list fvals r)))
nil)

(define-test hoop.5.43
(let ((vals '(0 0 1 1 0 1 1 1 0 0)))
(hoop* ((:each-elt etype in '(short-float single-float double-float long-float)
((:each-elt type = `(complex ,etype)
((:each-elt fvals = (hoop* ((:each-elt v in vals collect (coerce v type))
((:each-elt v = (make-array '(10) :initial-contents fvals :element-type type)
((:each-elt r = (hoop* ((:each-elt x :in v collect x)
unless (equal r fvals)
collect (list fvals r)))
nil)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(define-test hoop.5.44
(macrolet
((%m (z) z))
(hoop* ((:each-elt x :in (expand-in-current-env (%m "148X")) collect x))
(#\1 #\4 #\8 #\X))

(define-test hoop.5.45
(macrolet
((%m (z) z))
(hoop* as x :in (expand-in-current-env (%m #*00110110)) collect x))
(0 0 1 1 0 1 1 0))

;;; FIXME
;;; Add tests ((:each-elt other specialized array types (integer types, floats, complex)

;;; Error cases

(define-test hoop.5.error.1
(signals-error
(hoop* ((:each-elt (e . e) :in (vector '(x . y) '(u . v)) collect e)
program-error)
t)

(define-test hoop.5.error.2
(signals-error
(hoop* ((:each-elt e :in (vector '(x . y) '(u . v))
((:each-elt e from 1 to 5 collect e)
program-error)
t)

(define-test hoop.5.error.3
(signals-error
(macroexpand
'(hoop* ((:each-elt (e . e) :in (vector '(x . y) '(u . v)) collect e))
program-error)
t)

(define-test hoop.5.error.4
(signals-error
(macroexpand
'(hoop* ((:each-elt e :in (vector '(x . y) '(u . v))
((:each-elt e from 1 to 5 collect e))
program-error)
t)
|#
