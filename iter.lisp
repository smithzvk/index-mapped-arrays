
(in-package :ima)

;;<<>>=
(defun num-columns (ima)
  (ima-dimension ima 1))

;;<<>>=
(defclause-sequence in-column-vectors-of column-index
  :access-fn 'column-vector
  :size-fn 'num-columns
  :sequence-type t :element-type t)

;;<<>>=
(defun num-rows (ima)
  (ima-dimension ima 0))

;;<<>>=
(defclause-sequence in-row-vectors-of row-index
  :access-fn 'row-vector
  :size-fn 'num-rows
  :sequence-type t :element-type t)

;;<<>>=
(defclause-sequence in-ima ima-index
  :access-fn 'ima-flat-ref
  :size-fn 'ima-flat-size
  :sequence-type t :element-type t)
