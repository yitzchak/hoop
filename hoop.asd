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
         (:file "types"
          :depends-on ("packages"))
         (:file "declare"
          :depends-on ("packages"))
         (:file "clause"
          :depends-on ("packages"))
         (:file "accumulate"
          :depends-on ("clause"))
         (:file "execution"
          :depends-on ("clause"))
         (:file "generate"
          :depends-on ("clause"))
         (:file "hash-table"
          :depends-on ("clause"))
         (:file "list"
          :depends-on ("clause"))
         (:file "named"
          :depends-on ("clause"))
         (:file "numeric"
          :depends-on ("clause"))
         (:file "package"
          :depends-on ("clause"))
         (:file "return"
          :depends-on ("clause"))
         (:file "sequence"
          :depends-on ("clause"))
         (:file "step"
          :depends-on ("clause"
                       "declare"))
         (:file "termination"
          :depends-on ("clause"))
         (:file "with"
          :depends-on ("clause"))
         (:file "order"
          :depends-on ("clause"))
         (:file "analysis"
          :depends-on ("clause"))
         (:file "macro"
          :depends-on ("clause"
                       "declare"
                       "analysis"))))))

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
                :components ((:file "packages")
                             (:file "test"
                              :depends-on ("packages"))
                             (:file "hoop01"
                              :depends-on ("packages"))
                             (:file "hoop02"
                              :depends-on ("packages"))
                             (:file "hoop03"
                              :depends-on ("packages"))
                             (:file "hoop04"
                              :depends-on ("packages"))
                             (:file "hoop05"
                              :depends-on ("packages"))
                             (:file "hoop06"
                              :depends-on ("packages"
                                           "test"))
                             (:file "hoop07"
                              :depends-on ("packages"))
                             (:file "hoop08"
                              :depends-on ("packages"))
                             (:file "hoop09"
                              :depends-on ("packages"))
                             (:file "hoop10"
                              :depends-on ("packages"))
                             (:file "hoop11"
                              :depends-on ("packages"))
                             (:file "hoop12"
                              :depends-on ("packages"))
                             (:file "hoop13"
                              :depends-on ("packages"))
                             (:file "hoop14"
                              :depends-on ("packages"))
                             (:file "hoop15"
                              :depends-on ("packages"))
                             (:file "hoop16"
                              :depends-on ("packages"))
                             (:file "hoop17"
                              :depends-on ("packages"))))))
