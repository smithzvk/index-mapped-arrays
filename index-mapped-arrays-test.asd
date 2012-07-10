
(asdf:defsystem #:index-mapped-arrays-test
  :name "Index Mapped Arrays testsuite"
  :author "Zachary Smith <elzacho@gmail.com>"
  :license "LLGPL"
  :components ((:file "util")
               (:file "ima-test"))
  :serial t
  :depends-on (:iterate :index-mapped-arrays :stefil))

