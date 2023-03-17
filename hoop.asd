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
      :serial t
      :components
        ((:file "packages")
         (:file "hoop")
         (:file "list")
         (:file "collect")
         (:file "package")))))
