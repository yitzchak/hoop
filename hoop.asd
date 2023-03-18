(asdf:defsystem #:hoop
  :description "Common Lisp iteration"
  :author "Tarn W. Burton"
  :license "MIT"
  :version "0.1"
  :homepage "https://yitzchak.github.io/hoop/"
  :bug-tracker "https://github.com/yitzchak/hoop/issues"
  :depends-on (#:trivial-cltl2)
  :components
    ((:module code
      :components
        ((:file "packages")
         (:file "hoop" :depends-on ("packages"))
         (:file "arithmetic" :depends-on ("hoop"))
         (:file "list" :depends-on ("hoop"))
         (:file "accumulate" :depends-on ("hoop"))
         (:file "sequence" :depends-on ("hoop"))
         (:file "package" :depends-on ("hoop"))
         (:file "equals" :depends-on ("hoop"))
         (:file "generator" :depends-on ("hoop"))
         (:file "hash-table" :depends-on ("hoop"))))))
