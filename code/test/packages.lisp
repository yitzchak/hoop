(cl:in-package #:cl-user)

(defpackage #:hoop/test
  (:use #:common-lisp  #:parachute)
  (:import-from #:hoop
                #:hoop
                #:hoop*
                #:hoop-finish
                #:hoop-next))
