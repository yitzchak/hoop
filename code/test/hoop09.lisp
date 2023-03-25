(in-package #:hoop/test)

;;; Tests of COLLECT

(define-test hoop.9.1
  :compile-at :execute
  (is equal
      '(3 4 5)
      (hoop ((:each-item x :in '(2 3 4))
             (:collect c))
        (c (1+ x)))))

(define-test hoop.9.3
  :compile-at :execute
  (is equal
      'good
      (hoop ((:each-item x :in '(0 1 2))
             (:collect c))
        (when (eql x 2)
          (return 'good))
        (c x))))

(define-test hoop.9.4
  :compile-at :execute
  (is equal
      '((c) (b) (a))
      (hoop ((:each-item x :in '(a b c))
             (:collect foo)
             (:epilogue (return (reverse foo))))
        (foo (list x)))))

(define-test hoop.9.6
  :compile-at :execute
  (is equal
      '((2 4 6 8 10) (1 3 5 7 9))
      (hoop ((:step x :from 1 :to 10)
             (:collect foo)
             (:collect bar)
             (:epilogue (return (list foo bar))))
        (when (evenp x)
          (foo x))
        (when (oddp x)
          (bar x)))))

(define-test hoop.9.7
  :compile-at :execute
  (is equal
      '(1 2 3 4 5)
      (hoop ((:step x :from 1 :to 10)
             (:collect c))
        (if (> x 5)
            (hoop-finish)
            (c x)))))

(define-test hoop.9.8
  :compile-at :execute
  (is equal
      '(2 5 7 10 12 15 17 20)
      (hoop ((:step x :from 1 :to 20)
             (:collect foo))
        (when (eql (mod x 5) 0)
          (foo x))
        (when (eql (mod x 5) 2)
          (foo x)))))

(define-test hoop.9.10
  :compile-at :execute
  (fail-compile (hoop ((:with foo := '(a b))
                       (:each-item x :in '(c d))
                       (:collect foo))
                  (foo x))
                program-error))

(define-test hoop.9.12
  :compile-at :execute
  (is-values (let ((foo '(a b)))
               (values (hoop ((:each-item x :in '(c d e))
                              (:collect foo))
                         (foo x))
                       foo))
             (equal '(c d e))
             (equal '(a b))))

;;; Tests of APPEND, APPENDING

(define-test hoop.9.20
  :compile-at :execute
  (is equal
      '(a b c d e f g i)
      (hoop ((:each-item x :in '((a b) (c d) (e f g) () (i)))
             (:collect c))
        (c x :append))))

(define-test hoop.9.22
  :compile-at :execute
  (is equal
      '(a b c . whatever)      
      (hoop ((:each-item x :in '((a) (b) (c . whatever)))
             (:collect c))
        (c x :append))))

(define-test hoop.9.24
  :compile-at :execute
  (is equal
      '(a b 1 2 3 c d 4 5 6)
      (hoop ((:each-item x :in '(a b c d))
             (:collect c))
        (c (list x) :append)
        (when (eq x 'b)
          (c '(1 2 3) :append))
        (when (eq x 'd)
          (c '(4 5 6) :append)))))

(define-test hoop.9.25
  :compile-at :execute
  (is-values (let (z)
               (values (hoop ((:each-item x :in '((a) (b) (c) (d)))
                              (:return nil)
                              (:collect foo)
                              (:epilogue (setq z foo)))
                         (foo x :append))
                       z))
             (equal nil)
             (equal '(a b c d))))

(define-test hoop.9.26
  :compile-at :execute
  (is equal
      '(a a 1 b b 2 c c 3 d d 4)
      (hoop ((:each-item x :in '((a) (b) (c) (d)))
             (:step i :from 1)
             (:collect foo))
        (foo x :append)
        (foo x :append)
        (foo (list i) :append))))

(define-test hoop.9.27
  :compile-at :execute
  (fail-compile (hoop ((:with foo := '(a b))
                       (:each-item x :in '(c d))
                       (:collect foo))
                  (foo (list x) :append))
                program-error))

(define-test hoop.9.28
  :compile-at :execute
  (fail-compile (hoop ((:with foo := '(a b))
                       (:each-item x :in '(c d))
                       (:collect foo))
                  (foo (list x) :append))
                program-error))

;;; NCONC, NCONCING

#|(define-test hoop.9.30
  (hoop for x in '((a b) (c d) (e f g) () (i)) nconc (copy-seq x))
  (a b c d e f g i))

(define-test hoop.9.31
  (hoop for x in '((a b) (c d) (e f g) () (i)) nconcing (copy-seq x))
  (a b c d e f g i))

(define-test hoop.9.32
  (hoop for x in '((a) (b) (c . whatever)) nconc (cons (car x) (cdr x)))
  (a b c . whatever))

(define-test hoop.9.33
  (hoop for x in '((a) (b) (c . whatever)) nconcing (cons (car x) (cdr x)))
  (a b c . whatever))

(define-test hoop.9.34
  (hoop for x in '(a b c d)
        nconc (list x)
        when (eq x 'b) nconc (copy-seq '(1 2 3))
        when (eq x 'd) nconcing (copy-seq '(4 5 6)))
  (a b 1 2 3 c d 4 5 6))

(define-test hoop.9.35
  (let (z)
    (values
     (hoop for x in '((a) (b) (c) (d))
           nconc (copy-seq x) into foo
           finally (setq z foo))
     z))
  nil
  (a b c d))

(define-test hoop.9.36
  (hoop for x in '((a) (b) (c) (d))
        for i from 1
        nconc (copy-seq x) into foo
        nconc (copy-seq x) into foo
        nconcing (list i) into foo
        finally (return foo))
  (a a 1 b b 2 c c 3 d d 4))

(define-test hoop.9.37
  (signals-error
   (hoop with foo = '(a b)
         for x in '(c d) nconc (list x) into foo
         finally (return foo))
   program-error)
  t)

(define-test hoop.9.38
  (signals-error
   (hoop with foo = '(a b)
         for x in '(c d) nconcing (list x) into foo
         finally (return foo))
   program-error)
  t)

;;; Combinations

(define-test hoop.9.40
  (hoop for x in '(1 2 3 4 5 6 7)
        if (< x 2) append (list x)
        else if (< x 5) nconc (list (1+ x))
        else collect (+ x 2))
  (1 3 4 5 7 8 9))

(define-test hoop.9.41
  (hoop for x in '(1 2 3 4 5 6 7)
        if (< x 2) append (list x) into foo
        else if (< x 5) nconc (list (1+ x)) into foo
        else collect (+ x 2) into foo
        finally (return foo))
  (1 3 4 5 7 8 9))

;;; More nconc tests

(define-test hoop.9.42
  (hoop for x in '(a b c d e) nconc (cons x 'foo))
  (a b c d e . foo))

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(define-test hoop.9.43
  (macrolet
   ((%m (z) z))
   (hoop for x in '(1 2 3) collect (expand-in-current-env (%m (- x)))))
  (-1 -2 -3))

(define-test hoop.9.44
  (macrolet
   ((%m (z) z))
   (hoop for x in '(1 2 3) collecting (expand-in-current-env (%m (list x)))))
  ((1) (2) (3)))

(define-test hoop.9.45
  (macrolet
   ((%m (z) z))
   (hoop for x in '(a b c)
         collect (expand-in-current-env (%m (list x))) into foo
         finally (return (reverse foo))))
  ((c) (b) (a)))

(define-test hoop.9.46
  (macrolet
   ((%m (z) z))
   (hoop for x in '((a b) (c d) (e f g) () (i))
         append (expand-in-current-env (%m x))))
  (a b c d e f g i))

(define-test hoop.9.47
  (macrolet
   ((%m (z) z))
   (hoop for x in '((a b) (c d) (e f g) () (i))
         nconc (expand-in-current-env (%m (copy-seq x)))))
  (a b c d e f g i))
|#
