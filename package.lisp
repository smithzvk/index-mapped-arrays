
(in-package :cl-user)

;;<<>>=
(defpackage :index-mapped-arrays
  (:use :cl :iter :alexandria)
  (:nicknames :ima)
  (:export #:imref
           #:immod
           #:ima-dimension #:ima-dimensions
           #:backend-of #:map-of #:data-of #:index-mapped-array
           #:self-map #:contents-of
           ;; Getting pieces of IMAs
           #:get-vector #:column-vector #:row-vector
           #:get-diagonal
           #:get-slice
           #:get-block #:submatrix #:subvector
           #:transpose
           ;; Raising the dimensionality
           #:add-index #:group-elements-by
           ;; Grouping IMAs
           #:group-imas
           #:append-imas
           #:base-type-of
           #:unmap #:unmap-into #:def-unmapper
           #:make-ima-like #:copy-ima
           ;; Some useful mappings
           #:pbc-array
           #:index-shift
           #:nd-index
           #:linear-index
           ;; Iterate extensions
           #:in-column-vectors-of #:in-row-vectors-of
           #:in-ima
           ;; Mapping (like mapcar mapping)
           #:map-ima
           #:ensure-ima
           #:ima-p
           #:split-dimension
           #:combine-dimensions
           #:reduce-ima))

