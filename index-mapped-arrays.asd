
(asdf:defsystem :index-mapped-arrays
  :name "Index Mapped Arrays"
  :author "Zachary Smith <zachkostsmith@gmail.com>"
  :license "LLGPL"
  :description
  "This package is useful for working with multi-dimensional arrays,
for example pulling subspaces out of a larger structure and changing those
subspaces, all in a clean syntactically pleasing way.

Map tuples of indices to other tuples of indices.  These mappings can
be chained together, if wished.  At the lowest level the specified
interfacing functions allow for reading and writing memory associated
with the fundamental data structure and indices."
  :components ((:file "package")
               (:file "util")
               (:file "iter")
               (:file "index-mapped-arrays")
               (:file "arrays")
               (:file "lists")
               (:file "printer") )
  :serial t
  :depends-on (:toolbox :iterate) )


