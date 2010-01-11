
(asdf:defsystem #:index-mapped-arrays
  :name "Index Mapped Arrays"
  :author "Zachary Smith <elzacho@gmail.com>"
  :license "GPL"
  :description
  "This package is useful for working with multi-dimensional array,
for example pulling subspaces out of a larger structure in a clean
syntactically pleasing way.

Map tuples of indices to other tuples of indices.  These mappings can
be chained together, if wished.  At the lowest level the specified
interfacing functions allow for reading and writing memory associated
with the last indicies."
  :components ((:file "package")
               (:file "util")
               (:file "basic")
               (:file "gsl") )
  :serial t
  :depends-on (:alexandria :defclass-star :toolbox
                           :trivial-garbage :gsll :gsl-bindings ))

