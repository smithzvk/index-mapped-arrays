
(defpackage :index-mapped-arrays
    (:use :cl :iter :toolbox)
  (:nicknames :ima)
  (:export #:imref
           #:immod
           #:ima-dimension #:ima-dimensions
           #:backend-of #:map-of #:data-of #:index-mapped-array
           #:get-vector #:column-vector #:row-vector
           #:get-diagonal
           #:get-projection
           #:get-block #:submatrix
           #:transpose
           #:unmap #:unmap-into #:def-unmapper
           #:def-maker
           #:make-ima #:copy-ima
           ;; Iterate extensions
           #:in-column-vectors-of #:in-row-vectors-of
           #:in-ima ))

