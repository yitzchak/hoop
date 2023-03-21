(defpackage #:hoop
  (:use #:common-lisp)
  (:documentation "Common Lisp iteration")
  (:export #:block-name
           #:epilogue-forms
           #:before-forms
           #:after-forms
           #:termination-forms
           #:hoop
           #:hoop-finish
           #:hoop-next
           #:make-clause
           #:prologue-forms
           #:wrap-form))
