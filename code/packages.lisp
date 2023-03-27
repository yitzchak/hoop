(defpackage #:hoop
  (:use #:common-lisp)
  (:documentation "Common Lisp iteration")
  (:export #:after-forms
           #:before-forms
           #:block-name
           #:epilogue-forms
           #:hoop
           #:hoop*
           #:hoop-finish
           #:hoop-next
           #:initial-early-forms
           #:initial-late-forms
           #:initial-movable-forms
           #:make-clause
           #:next-early-forms
           #:next-late-forms
           #:next-movable-forms
           #:prologue-forms
           #:wrap-inner-form
           #:wrap-outer-form))
