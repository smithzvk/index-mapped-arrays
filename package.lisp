

(defpackage :index-mapped-arrays
  (:use :cl :tb :alexandria :defclass-star)
  (:shadow #:with-gensyms #:shuffle)
  (:nicknames :ima)
  (:export #:index-mapped-array
           #:make-index-mapped-array
           #:construct-index-mapped-array
           #:imref
           #:ima-dimensions
           #:ima-dimension
           ;; Apply maps
           #:remap-indices
           #:map-indices
           ;; General mapping generators
           #:identity-map
           #:array-hyperplane-mapping
           #:vector-mapping
           #:submatrix-mapping
           #:row-major-mapping
           ;; Convenience functions
           #:get-vector
           #:get-slice
           #:get-subarray
           #:column-vector
           #:row-vector
           ;; Lisp array conversions
           #:imarray<-array
           #:array<-imarray
           ;; Tree/list conversions
           #:tree<-imarray
           #:copy-ima
           ;; Matrix ops (optimized 2D arrays)
           #:row-vector
           #:column-vector
           #:submatrix
           #:transpose ))
