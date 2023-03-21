(asdf:defsystem #:hoop
  :description "Common Lisp iteration"
  :author "Tarn W. Burton"
  :license "MIT"
  :version "0.1"
  :homepage "https://yitzchak.github.io/hoop/"
  :bug-tracker "https://github.com/yitzchak/hoop/issues"
  :in-order-to ((asdf:test-op (asdf:test-op #:hoop/test)))
  :components
    ((:module code
      :components
        ((:file "packages")
         (:file "clause" :depends-on ("packages"))
         (:file "accumulate" :depends-on ("clause"))
         (:file "execution" :depends-on ("clause"))
         (:file "generate" :depends-on ("clause"))
         (:file "hash-table" :depends-on ("clause"))
         (:file "list" :depends-on ("clause"))
         (:file "named" :depends-on ("clause"))
         (:file "numeric" :depends-on ("clause"))
         (:file "package" :depends-on ("clause"))
         (:file "return" :depends-on ("clause"))
         (:file "sequence" :depends-on ("clause"))
         (:file "step" :depends-on ("clause"))
         (:file "termination" :depends-on ("clause"))
         (:file "with" :depends-on ("clause"))
         (:file "macro" :depends-on ("clause"))))))

(asdf:defsystem :hoop/test
  :description "Test system for Hoop"
  :license "MIT"
  :license "MIT"
  :version "0.1"
  :homepage "https://yitzchak.github.io/hoop/"
  :bug-tracker "https://github.com/yitzchak/hoop/issues"
  :depends-on (:hoop :parachute)
  :perform (asdf:test-op (op c)
             (defparameter cl-user::*exit-on-test-failures* t)
             (uiop:symbol-call :parachute :test :hoop/test))
  :components ((:module code
                :pathname "code/test/"
                :serial t
                :components ((:file "packages")
                             (:file "test")
                             (:file "hoop1")))))
