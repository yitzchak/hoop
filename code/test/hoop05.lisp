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

(define-test hoop.5.4
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

(define-test hoop.5.6
  :compile-at :execute
  (is equal
      '(#\a #\b #\c)
      (hoop* ((:each-elt e :in "abcd" :end 3)
              (:collect c))
        (declare (type (or null base-char) e))
        (c e))))

(define-test hoop.5.7
  :compile-at :execute
  (is equal
      '(#\a #\b #\c #\d)
      (let ((x (make-array '(4) :initial-contents "abcd" :element-type 'base-char)))
        (hoop* ((:each-elt e :in (the base-string x))
                (:collect foo))
          (foo e)))))

(define-test hoop.5.8
  :compile-at :execute
  (is equal
      '(#\a #\b #\c #\d)
      (let ((x (make-array '(4) :initial-contents "abcd" :element-type 'base-char)))
        (hoop* ((:each-elt e :in (the base-string x))
                (:collect foo))
          (declare (type character e))
          (foo e)))))

(define-test hoop.5.10
  :compile-at :execute
  (is equal
      '(0 0 0 1 0 1 1 0)
      (let ((x #*00010110))
        (hoop* ((:each-elt e :in x)
                (:collect foo))
          (foo e)))))

(define-test hoop.5.11
  :compile-at :execute
  (is equal
      '(0 0 0 1 0 1 1 0)
      (let ((x #*00010110))
        (hoop* ((:each-elt e :in (the bit-vector x))
                (:collect foo))
          (foo e)))))

(define-test hoop.5.12
  :compile-at :execute
  (is equal
      '(0 0 0 1 0 1 1 0)
      (let ((x #*00010110))
        (hoop* ((:each-elt e :in (the simple-bit-vector x))
                (:collect foo))
          (foo e)))))

(define-test hoop.5.13
  :compile-at :execute
  (is equal
      '(0 0 0 1 0 1 1 0)
      (let ((x #*00010110))
        (hoop* ((:each-elt e :in (the simple-bit-vector x))
                (:collect foo))
          (declare (type bit e))
          (foo e)))))

(define-test hoop.5.14
  :compile-at :execute
  (is equal
      '(0 0 0 1)
      (let ((x #*00010110))
        (hoop* ((:each-elt e :in x :end 4)
                (:collect foo))
          (declare (type bit e))
          (foo e)))))

(define-test hoop.5.20
  (is equal
      '(a b c d)
      (let ((x (vector 'a 'b 'c 'd)))
        (hoop* ((:each-elt e :in x)
                (:collect foo))
          (foo e)))))

(define-test hoop.5.21
  :compile-at :execute
  (is equal
      '(a b c d)
      (let ((x (vector 'a 'b 'c 'd)))
        (hoop* ((:each-elt e :in (the vector x))
                (:collect foo))
          (foo e)))))

(define-test hoop.5.22
  :compile-at :execute
  (is equal
      '(a b c d)
      (let ((x (vector 'a 'b 'c 'd)))
        (hoop* ((:each-elt e :in (the simple-vector x))
                (:collect foo))
          (foo e)))))

(define-test hoop.5.23
  :compile-at :execute
  (is equal
      '(a b c d)
      (let ((x (vector '(a) '(b) '(c) '(d))))
        (hoop* ((:each-elt (e) :in x)
                (:collect foo))
          (foo e)))))

(define-test hoop.5.30
  :compile-at :execute
  (is equal
      '(a b c d e)
      (let ((x (make-array '(5) :initial-contents '(a b c d e)
                                :adjustable t)))
        (hoop* ((:each-elt e :in x)
                (:collect foo))
          (foo e)))))

(define-test hoop.5.32
  :compile-at :execute
  (is equal
      '(b c d)
      (let* ((x (make-array '(5) :initial-contents '(a b c d e)))
             (y (make-array '(3) :displaced-to x
                                 :displaced-index-offset 1)))
        (hoop* ((:each-elt e :in y)
                (:collect foo))
          (foo e)))))

;;; hoop.5.33 thru hoop.5.35 skipped since HOOP doesn't have AS
;;; tests of 'as' ((:each-eltm

;;; Hoop :in displaced vectors

(define-test hoop.5.36
  :compile-at :execute
  (is equal
      '(c d e f g)
      (let* ((a (make-array '(10) :initial-contents '(a b c d e f g h i j)))
             (da (make-array '(5) :displaced-to a
                                  :displaced-index-offset 2)))
        (hoop* ((:each-elt e :in da)
                (:collect foo))
          (foo e)))))

(define-test hoop.5.37
  :compile-at :execute
  (is equal
      '(#\c #\d #\e #\f #\g)
      (let* ((a (make-array '(10) :element-type 'base-char
                                  :initial-contents "abcdefghij"))
             (da (make-array '(5) :element-type 'base-char
                                  :displaced-to a
                                  :displaced-index-offset 2)))
        (hoop* ((:each-elt e :in da)
                (:collect foo))
          (foo e)))))

(define-test hoop.5.38
  :compile-at :execute
  (is equal
      '(1 0 0 1 0)
      (let* ((a (make-array '(10) :element-type 'bit
                                  :initial-contents '(0 1 1 0 0 1 0 1 1 1)))
             (da (make-array '(5) :element-type 'bit
                                  :displaced-to a
                                  :displaced-index-offset 2)))
        (hoop* ((:each-elt e :in da)
                (:collect foo))
          (foo e)))))

(define-test hoop.5.39
  :compile-at :execute
  (is equal
      '(1 2 3 4 5 6)
      (let ((v (make-array '(10) :initial-contents '(1 2 3 4 5 6 7 8 9 10)
                                 :fill-pointer 6)))
        (hoop* ((:each-elt x :in v)
                (:collect foo))
          (foo x)))))

(define-test hoop.5.40
  :compile-at :execute
  (false (hoop* ((:step i :from 1 :to 40)
                 (:generate type :using `(unsigned-byte ,i))
                 (:generate v :using (make-array '(10) :initial-contents '(0 0 1 1 0 1 1 1 0 0)
                                                       :element-type type))
                 (:generate r :using (hoop* ((:each-elt x :in v)                                                                            (:collect foo))
                                       (foo x)))
                 (:collect bar))
           (unless (equal r '(0 0 1 1 0 1 1 1 0 0))
             (bar (list i r))))))

(define-test hoop.5.41
  :compile-at :execute
  (false (hoop* ((:step i :from 1 :to 40)
                 (:generate type :using `(signed-byte ,i))
                 (:generate v :using (make-array '(10) :initial-contents '(0 0 -1 -1 0 -1 -1 -1 0 0)
                                                       :element-type type))
                 (:generate r :using (hoop* ((:each-elt x :in v)
                                             (:collect foo))
                                       (foo x)))
                 (:collect bar))
           (unless (equal r '(0 0 -1 -1 0 -1 -1 -1 0 0))
             (bar (list i r))))))

(define-test hoop.5.42
  :compile-at :execute
  (false (let ((vals '(0 0 1 1 0 1 1 1 0 0)))
           (hoop* ((:each-elt type :in '(short-float single-float double-float long-float))
                   (:generate fvals :using (hoop* ((:each-elt v :in vals)
                                                   (:collect foo))
                                             (foo (coerce v type))))
                   (:generate v :using (make-array '(10) :initial-contents fvals :element-type type))
                   (:generate r :using (hoop* ((:each-elt x :in v)
                                               (:collect foo))
                                         (foo x)))
                   (:collect bar))
             (unless (equal r fvals)
               (bar (list fvals r)))))))

(define-test hoop.5.43
  :compile-at :execute
  (false (let ((vals '(0 0 1 1 0 1 1 1 0 0)))
           (hoop* ((:each-elt etype :in '(short-float single-float double-float long-float))
                   (:generate type :using `(complex ,etype))
                   (:generate fvals :using (hoop* ((:each-elt v :in vals)
                                                   (:collect foo))
                                             (foo (coerce v type))))
                   (:generate v :using (make-array '(10) :initial-contents fvals :element-type type))
                   (:generate r :using (hoop* ((:each-elt x :in v)
                                               (:collect foo))
                                         (foo x)))
                   (:collect bar))
             (unless (equal r fvals)
               (bar (list fvals r)))))))

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(define-test hoop.5.44
  :compile-at :execute
  (is equal
      '(#\1 #\4 #\8 #\X)
      (macrolet ((%m (z) z))
        (hoop* ((:each-elt x :in (expand-in-current-env (%m "148X")))
                (:collect foo))
          (foo x)))))

;;; Skip hoop.5.45 since it tests the same feature as hoop.5.44

;;; FIXME
;;; Add tests ((:each-elt other specialized array types (integer types, floats, complex)

;;; Error cases

(define-test hoop.5.error.1
  :compile-at :execute
  (fail-compile (hoop* ((:each-elt (e . e) :in (vector '(x . y) '(u . v)))
                        (:collect foo))
                  (foo e))
                program-error))

(define-test hoop.5.error.2
  :compile-at :execute
  (fail-compile (hoop* ((:each-elt e :in (vector '(x . y) '(u . v)))
                        (:step e :from 1 :to 5)
                        (:collect foo))
                  (foo e))
                program-error))

(define-test hoop.5.error.3
  :compile-at :execute
  (fail (macroexpand '(hoop* ((:each-elt (e . e) :in (vector '(x . y) '(u . v)))
                              (:collect foo))
                        (foo e)))
        program-error))

(define-test hoop.5.error.4
  :compile-at :execute
  (fail (macroexpand '(hoop* ((:each-elt e :in (vector '(x . y) '(u . v)))
                              (:step e :from 1 :to 5)
                              (:collect foo))
                        (foo e)))
        program-error))
