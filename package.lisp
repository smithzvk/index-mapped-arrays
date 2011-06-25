
(defpackage :index-mapped-arrays
    (:use :cl :iter :alexandria :cl-match)
  (:nicknames :ima)
  (:export #:imref
           #:immod
           #:ima-dimension #:ima-dimensions
           #:backend-of #:map-of #:data-of #:index-mapped-array
           #:get-vector #:column-vector #:row-vector
           #:get-diagonal
           #:get-projection
           #:get-block #:submatrix #:subvector
           #:transpose
           #:self-map #:contents-of
           #:unmap #:unmap-into #:def-unmapper
           #:make-ima-like #:copy-ima
           ;; Iterate extensions
           #:in-column-vectors-of #:in-row-vectors-of
           #:in-ima ))

