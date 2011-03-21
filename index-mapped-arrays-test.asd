
(asdf:defsystem #:index-mapped-arrays-test
  :name "Index Mapped Arrays testsuite"
  :author "Zachary Smith <elzacho@gmail.com>"
  :license "GPL"
  :components ((:file "ima-test") )
  :serial t
  :depends-on (:toolbox :iterate :index-mapped-arrays :stefil) )

