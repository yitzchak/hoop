(in-package #:hoop/test)


(defpackage "HOOP.CL-TEST.1"
  (:use)
  (:intern "FOO" "BAR" "BAZ")
  (:export "A" "B" "C"))

(defpackage "HOOP.CL-TEST.2"
  (:use "HOOP.CL-TEST.1")
  (:intern "X" "Y" "Z"))

(define-test hoop.7.1
  :compile-at :execute
  (is equal
      '("A" "B" "BAR" "BAZ" "C" "FOO")
      (sort (mapcar #'symbol-name
                    (hoop ((:each-symbol x :in "HOOP.CL-TEST.1"
                            :symbol-types (:external :internal))
                           (:collect c))
                      (c x)))
            #'string<)))

(define-test hoop.7.9
  :compile-at :execute
  (is equal
      '("A" "B" "C")
      (sort (mapcar #'symbol-name
                    (hoop ((:each-symbol x :in "HOOP.CL-TEST.1")
                           (:collect c))
                      (c x)))
            #'string<)))

(define-test hoop.7.11
  :compile-at :execute
  (is equal
      '("A" "B" "C")
      (sort (mapcar #'symbol-name
                    (hoop ((:each-symbol x :in (find-package "HOOP.CL-TEST.1"))
                           (:collect c))
                      (c x)))
            #'string<)))

(define-test hoop.7.12
  :compile-at :execute
  (is equal
      '("A" "B" "C")
      (sort (mapcar #'symbol-name
                    (hoop ((:each-symbol x :in :HOOP.CL-TEST.1)
                           (:collect c))
                      (c x)))
            #'string<)))

(define-test hoop.7.13
  :compile-at :execute
  (is equal
      '("A" "B" "C" "X" "Y" "Z")
      (sort (mapcar #'symbol-name
                    (hoop ((:each-symbol x :in "HOOP.CL-TEST.2"
                            :symbol-types (:external :internal :inherited))
                           (:collect c))
                      (c x)))
            #'string<)))

(define-test hoop.7.14
  :compile-at :execute
  (is equal
      '("X" "Y" "Z")
      (sort (mapcar #'symbol-name
                    (hoop ((:each-symbol x :in "HOOP.CL-TEST.2"
                            :symbol-types (:external :internal))
                           (:collect c))
                      (c x)))
            #'string<)))

;;; According to the ANSI CL spec, "If the package for the iteration
;;; is not supplied, the current package is used."  Thse next tests
;;; are of the cases that the package is not supplied in the hoop
;;; form.

(define-test hoop.7.15
  :compile-at :execute
  (is equal
      '("A" "B" "BAR" "BAZ" "C" "FOO")
      (let ((*package* (find-package "HOOP.CL-TEST.1")))
        (sort (mapcar #'symbol-name (hoop ((:each-symbol x
                                            :symbol-types (:external :internal))
                                           (:collect c))
                                      (c x)))
              #'string<))))

(define-test hoop.7.16
  :compile-at :execute
  (is equal
      '("A" "B" "C")
      (let ((*package* (find-package "HOOP.CL-TEST.1")))
        (sort (mapcar #'symbol-name (hoop ((:each-symbol x)
                                           (:collect c))
                                      (c x)))
              #'string<))))

(define-test hoop.7.16
  :compile-at :execute
  (is equal
      '("X" "Y" "Z")
      (let ((*package* (find-package "HOOP.CL-TEST.2")))
        (sort (mapcar #'symbol-name (hoop ((:each-symbol x
                                            :symbol-types (:external :internal))
                                           (:collect c))
                                      (c x)))
              #'string<))))

;;; Cases where the package doesn't exist.  According to the standard,
;;; (section 6.1.2.1.7), this should cause a package-error.

(define-test hoop.7.18
  :compile-at :execute
  (ignore-errors (delete-package "HOOP.MISSING.PACKAGE"))
  (fail (hoop ((:each-symbol x :in "HOOP.MISSING.PACKAGE"
                :symbol-types (:external :internal))
               (:collect c))
          (c x))
        package-error))

(define-test hoop.7.19
  :compile-at :execute
  (ignore-errors (delete-package "HOOP.MISSING.PACKAGE"))
  (fail (hoop ((:each-symbol x :in "HOOP.MISSING.PACKAGE"
                :symbol-types (:external :internal :inherited))
               (:collect c))
          (c x))
        package-error))

(define-test hoop.7.20
  :compile-at :execute
  (ignore-errors (delete-package "HOOP.MISSING.PACKAGE"))
  (fail (hoop ((:each-symbol x :in "HOOP.MISSING.PACKAGE")
               (:collect c))
          (c x))
        package-error))

;;; NIL d-var-specs

(define-test hoop.7.21
  :compile-at :execute
  (is equal
      6
      (hoop ((:each-symbol nil :in "HOOP.CL-TEST.1"
              :symbol-types (:external :internal))
             (:count c))
            (c t))))

(define-test hoop.7.22
  :compile-at :execute
  (is equal
      3
      (hoop ((:each-symbol nil :in "HOOP.CL-TEST.1")
             (:count c))
            (c t))))

(define-test hoop.7.23
  :compile-at :execute
  (is equal
      3
      (hoop ((:each-symbol nil :in "HOOP.CL-TEST.2"
              :symbol-types (:external :internal))
             (:count c))
            (c t))))

;;; Type specs

#|(define-test hoop.7.24
  (hoop for x t being the symbols of "HOOP.CL-TEST.1" count x)
  6)

(define-test hoop.7.25
  (hoop for x t being the external-symbols of "HOOP.CL-TEST.1" count x)
  3)

(define-test hoop.7.26
  (hoop for x t being the present-symbols of "HOOP.CL-TEST.2" count x)
  3)

(define-test hoop.7.27
  (hoop for x of-type symbol being the symbols of "HOOP.CL-TEST.1" count x)
  6)

(define-test hoop.7.28
  (hoop for x of-type symbol being the external-symbols of "HOOP.CL-TEST.1" count x)
  3)

(define-test hoop.7.29
  (hoop for x of-type symbol being the present-symbols of "HOOP.CL-TEST.2" count x)
  3)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(define-test hoop.7.33
  (macrolet
   ((%m (z) z))
   (sort (mapcar #'symbol-name
                 (hoop for x being the symbols of
                       (expand-in-current-env (%m "HOOP.CL-TEST.1"))
                       collect x))
         #'string<))
  ("A" "B" "BAR" "BAZ" "C" "FOO"))

(define-test hoop.7.34
  (macrolet
   ((%m (z) z))
   (sort (mapcar #'symbol-name
                 (hoop for x being the external-symbols of
                       (expand-in-current-env (%m "HOOP.CL-TEST.1"))
                       collect x))
         #'string<))
  ("A" "B" "C"))

(define-test hoop.7.35
  (macrolet
   ((%m (z) z))
   (sort (mapcar #'symbol-name
                 (hoop for x being the present-symbols of
                       (expand-in-current-env (%m "HOOP.CL-TEST.2"))
                       collect x))
         #'string<))
  ("X" "Y" "Z"))
|#