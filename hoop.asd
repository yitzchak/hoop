(asdf:defsystem #:hoop
  :description "Common Lisp iteration"
  :author "Tarn W. Burton"
  :license "MIT"
  :version "0.1"
  :homepage "https://yitzchak.github.io/hoop/"
  :bug-tracker "https://github.com/yitzchak/hoop/issues"
  :components
    ((:module code
      :components
        ((:file "packages")
         (:file "clause" :depends-on ("packages"))
         (:file "step" :depends-on ("clause"))
         (:file "list" :depends-on ("clause"))
         (:file "numeric" :depends-on ("clause"))
         (:file "return" :depends-on ("clause"))
         (:file "accumulate" :depends-on ("clause"))
         (:file "sequence" :depends-on ("clause"))
         (:file "package" :depends-on ("clause"))
         (:file "with" :depends-on ("clause"))
         (:file "generator" :depends-on ("clause"))
         (:file "hash-table" :depends-on ("clause"))
         (:file "termination" :depends-on ("clause"))
         (:file "execution" :depends-on ("clause"))
         (:file "macro" :depends-on ("clause"))))))
