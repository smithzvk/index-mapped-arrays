
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
           #:in-ima )
  ;; (:export
;;    ;; General mapping generators
;;            #:array-hyperplane-mapping
;;            #:vector-mapping
;;            #:submatrix-mapping
;;            #:row-major-mapping
;;            ;; Convenience functions
;;            #:copy-ima
 )

;; (defpackage :index-mapped-arrays
;;   (:use :cl :tb :alexandria :defclass-star)
;;   (:shadow #:with-gensyms #:shuffle)
;;   (:nicknames :ima)
;;   (:export #:index-mapped-array
;;            #:imref
;;            #:ima-dimensions
;;            #:ima-dimension
;;            ;; Apply maps
;;            #:remap-indices
;;            #:map-indices
;;            ;; General mapping generators
;;            #:identity-map
;;            #:array-hyperplane-mapping
;;            #:vector-mapping
;;            #:submatrix-mapping
;;            #:row-major-mapping
;;            ;; Convenience functions
;;            #:get-vector
;;            #:get-slice
;;            #:get-subarray
;;            #:column-vector
;;            #:row-vector
;;            ;; Lisp array conversions
;;            #:imarray<-array
;;            #:array<-imarray
;;            ;; Tree/list conversions
;;            #:tree<-imarray
;;            #:copy-ima
;;            ;; Matrix ops (optimized 2D arrays)
;;            #:row-vector
;;            #:column-vector
;;            #:submatrix
;;            #:transpose ))
