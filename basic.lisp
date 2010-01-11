;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; An implementation of index mapping on general structures.
;;;;
;;;; You specify the structure, how to access/set elements and how to
;;;; map indices to the actual data.  See examples.lisp,
;;;; doc/index-mapped-arrays.pdf, or the Wikipedia page on this for
;;;; reasons why you might like this
;;;;
;;;; This code is released under the GPL.

(in-package :index-mapped-arrays)

;;;;;;;;;;;;;;;;;
;;; The Structure

;;; Some imp-specific stuff could be done here (i.e. SBCL can take
;;; meaningful real values, and of course these mean different things
;;; from imp to imp)
;; Development optimizations
(declaim (optimize (speed 2) (debug 3)))
;; Production optimizaitons
;(declaim (optimize (speed 2) (debug 0) (safety 0) (compilation-speed 0)))

(defstruct (index-mapped-array
             (:constructor
              construct-index-mapped-array (arr dims params map) )
             (:print-object pprint-imarray)
             (:conc-name ima-) )
  "A structure that wraps a data object and functions that know how to
map a set of indices onto parts of the object.

   ARR: some data object
  DIMS: A list of dimensions
PARAMS: Parameters that may be needed for 
   MAP: A function that transforms its arguments into something that
        the method imref for this type understands"
  (arr ; Contains the data
   #() :type vector )
  (dims '(3 3) :type list)  ; A list of dimensions
  (params '())
  ;; A function that maps indices onto one index.  This function takes
  ;; any number of arguments.
  (map (identity-map '(3 3)) :type (function * fixnum)) )

(defstruct (affine-mapped-matrix
             (:constructor
              create-affine-mapped-matrix (mat dims a b) )
             ;(:print-object pprint-imarray)
             (:conc-name amm-) )
  "A structure that wraps a data object and functions that know how to
map a set of indices onto parts of the object.

   MAT: some data object
  DIMS: A list of dimensions
     A: A 2x2 matrix that transforms the index vector
     B: A length 2 vector that translates the transformed index vector

          idx' = A . idx + b"
  (mat ; Contains the data
   #() :type vector )
  (dims '(3 3) :type list)  ; A list of dimensions
  (a #(1 0 0 1) :type (array integer (4)))
  (b #(0 0) :type (array integer (2))) )

(defmethod imref ((mat affine-mapped-matrix) &rest idx)
  (declare (dynamic-extent idx)
           (optimize (speed 3)) )
  (destructuring-bind (i j) idx
    (svref (amm-mat mat)
           (+ (* (car (amm-dims mat))
                 (+ (* (aref (amm-a mat) 0) i)
                    (* (aref (amm-a mat) 1) j)
                    (aref (amm-b mat) 0) ))
              (+ (* (aref (amm-a mat) 2) i)
                 (* (aref (amm-a mat) 3) j)
                 (aref (amm-b mat) 1) )))))

(defmethod (setf imref) (val (mat affine-mapped-matrix) &rest idx)
  (declare (dynamic-extent idx)
           (optimize (speed 3)) )
  (destructuring-bind (i j) idx
    (setf (svref (amm-mat mat)
                 (+ (* (car (amm-dims mat))
                       (+ (* (aref (amm-a mat) 0) i)
                          (* (aref (amm-a mat) 1) j)
                          (aref (amm-b mat) 0) ))
                    (+ (* (aref (amm-a mat) 2) i)
                       (* (aref (amm-a mat) 3) j)
                       (aref (amm-b mat) 1) )))
          val )))

;;;;;;;;;;;;;;;;;
;;; The interface

(defmethod imref ((arr index-mapped-array) &rest idx)
  "Access whatever piece of the `ARR' slot of ARR that the slot `ELT'
interprets the application of the slot `MAP' onto `IDX' to mean."
  (declare (dynamic-extent idx)
           (optimize (speed 3)) )
  (aref (ima-arr arr) (apply (the function (ima-map arr)) idx)) )

(defmethod (setf imref) (val (arr index-mapped-array) &rest idx)
  "Set whatever piece of the `ARR' slot of ARR that the slot
`ELT' interprets the application of the slot `MAP' onto IDX to
mean to VAL."
  (declare (dynamic-extent idx)
           (optimize (speed 3)) )
  (setf (aref (ima-arr arr) (apply (the function (ima-map arr)) idx))
        val ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create mapped structures

(defun remap-indices (arr dims params map)
  "Create a new INDEX-MAPPED-ARRAY structure based on ARR with
dimensions DIMS replaing the old mapping with MAP"
  (construct-index-mapped-array
   (ima-arr arr)
   dims
   params
   map ))

(defun map-indices (arr dims params map)
  "Create a new INDEX-MAPPED-ARRAY structure based on ARR with
dimensions DIMS replaing the old mapping with the composition of the
old map and MAP"
  (construct-index-mapped-array
   (ima-arr arr)
   dims
   params
   (multiple-value-compose
    (the function (ima-map arr))
    (the function map) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some convenient mapping generators

;;; These generate quite general, albeit consing, mapping functions.

;;; Due to the chaining and the use of multiple indices at each level,
;;; these maps cons on every call.  This could (does) hurt
;;; performance.

(defun identity-map (dims)
  "Returns a function that maps n-DIMS indices onto one index."
  (let ((map (append (maplist (curry #'apply #'*) (cdr dims)) '(1))))
    (/. (&rest args)
      (apply #'+ (mapcar #'* args map)) )))

(defun array-hyperplane-mapping (value index)
  "For array dimension D, return a function that maps D-1 indices onto
the D indices by fixing the Nth index to value INDEX."
  (/. (&rest idx)
    (apply #'values (list-insert-at index value idx))) )

(defun vector-mapping (n &rest index)
  "Returns a function that maps to a single vector in the array.
i.e. allow the Nth index to vary, fix all others to the values in
INDEX."
  (multiple-value-compose
   (curry #'arg-fiddle-mv
          (cons n (map0-n #'identity
                          (1- (length index)) )))
   (/. (i) (apply #'values i index)) ))

(defun subarray-mapping (&rest start)
  "Returns a function that maps to a consecutive subspace of the
array.  The subspace starts at indices START."
  (/. (&rest idx)
    (values-list (mapcar #'+ start idx))) )

(defun row-major-mapping (dims)
  (let ((counts (append (cdr (maplist (curry #'apply #'*) dims)) '(1))))
    (labels ((get-indices (i counts)
               (cond ((null counts) nil)
                     (t (cons (floor i (car counts))
                              (get-indices (rem i (car counts))
                                           (cdr counts) ))))))
      (/. (i)
        (values-list (get-indices i counts))) )))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; High level interface

(defun get-vector (arr n
                   &rest fixed
                   &aux (dims (ima-dimensions arr)) )
  (map-indices arr (list (nth n dims)) '()
               (apply #'vector-mapping n fixed) ))

(defun get-slice (arr value index)
  (map-indices arr
               (list-remove-at index (ima-dims arr))
               '()
               (array-hyperplane-mapping value index) ))

(defmethod get-subarray (arr &rest ranges)
  (let ((start (mapcar #'car ranges))
        (dims (mapcar (compose #'- (curry #'apply #'-)) ranges)) )
    (map-indices arr dims '() (apply #'subarray-mapping start)) ))

(defun transpose (mat)
  (map-indices mat (ima-dims mat) '() (/. (i j) (values j i))) )

(defmethod column-vector ((arr index-mapped-array) nth)
  (get-vector arr 0 nth) )

(defmethod row-vector ((arr index-mapped-array) nth)
  (get-vector arr 1 nth) )

;;;;;;;;;;;;;;;;;;;;;;
;;; Lisp array support

(defmethod imref ((mat array) &rest idx)
  (apply #'aref mat idx) )
(defmethod (setf imref) (val (mat array) &rest idx)
  (setf (apply #'aref mat idx) val) )

(defun make-index-mapped-array (dims &rest rest)
  (awhen (position :initial-contents rest)
    (setf (nth (1+ it) rest) (flatten (nth (1+ it) rest))) )
  (when (not (listp dims))
    (setf dims (list dims)) )
  (construct-index-mapped-array
   (apply #'make-array (apply #'* dims) rest)
   dims
   '()
   (identity-map dims) ))

(defmethod ima-dimensions ((arr index-mapped-array))
  (ima-dims arr) )

(defmethod ima-dimension ((arr index-mapped-array) n)
  (nth n (ima-dims arr)) )

;; (defun copy-imarray (array &key 
;;                      (element-type (array-element-type array))
;;                      (fill-pointer (and (array-has-fill-pointer-p array)
;;                                         (fill-pointer array)))
;;                      (adjustable (adjustable-array-p array)))
;;   "Returns an undisplaced copy of ARRAY, with same fill-pointer
;; and adjustability (if any) as the original, unless overridden by
;; the keyword arguments."
;;   (let ((dims (array-dimensions array)))
;;     ;; Dictionary entry for ADJUST-ARRAY requires adjusting a
;;     ;; displaced array to a non-displaced one to make a copy.
;;     (adjust-array
;;      (make-array dims 
;;                  :element-type element-type :fill-pointer fill-pointer
;;                  :adjustable adjustable :displaced-to array)
;;      dims)))

(defun imarray<-array (arr)
  (construct-index-mapped-array
   (make-array (apply #'* (array-dimensions arr)) :displaced-to arr)
   (array-dimensions arr)
   '()
   (identity-map (array-dimensions arr)) ))

(defun array<-imarray (imarr)
  (let* ((dims (ima-dims imarr))
         (map (row-major-mapping dims))
         (imarr (map-indices imarr (list (apply #'* dims)) '() map))
         (arr (make-array (apply #'* dims))) )
    (dotimes (i (apply #'* dims) (make-array dims :displaced-to arr))
      (setf (aref arr i) (imref imarr i)) )))

(defun tree<-imarray (arr)
  (cond ((= 1 (length (ima-dimensions arr)))
         (let (ret)
           (dotimes (i (ima-dimension arr 0) (reverse ret))
             (push (imref arr i) ret) )))
        (t (let (ret)
             (dotimes (i (ima-dimension arr 0) (reverse ret))
               (push (tree<-imarray
                      (get-slice arr i 0)) ret ))))))

(defun pprint-imarray (array stream)
  "Print the index mapped ARRAY to STREAM using the pretty printer."
  ;; Apapted from the SBCL printer
  (funcall (formatter "#~DD-IMA") stream (length (ima-dimensions array)))
  (labels ((output-guts (stream array)
             (pprint-logical-block (stream nil :prefix "(" :suffix ")")
               (dotimes (i (ima-dimension array 0))
                 (when (not (= i 0)) (write-char #\Space stream)
                       (pprint-newline (if (ima-dimension array 0) :linear :fill) stream) )
                 (if (= 1 (length (ima-dimensions array)))
                     (format stream "~A" (imref array i))
                     (output-guts stream (get-slice array i 0)) )))))
    (output-guts stream array) ))

(defun print-imarray (arr str)
  (format str "IM")
  (write (array<-imarray arr)
         :lines 8
         :length 8
         :stream str ))

(defmethod copy-ima ((arr index-mapped-array))
  "This makes a copy of the array and a copy of the map.  Thus we get
an identical thing coming out."
  (construct-index-mapped-array
   (copy-array (ima-arr arr))
   (ima-dims arr)
   nil
   (ima-map arr) ))

(defun flatten-ima-map (arr)
  "This goes through each element of the array and copies it into a
new array, making an object where indicies reference the same object
as in the original, but the map of the new function is just the
identity function."
  (let ((new-arr (make-index-mapped-array
                  (ima-dims arr) )))
    (labels ((%flatten (idx ranges)
               (if ranges
                   (dotimes (i (car ranges))
                     (%flatten (cons i idx) (cdr ranges)) )
                   (setf (apply #'imref new-arr idx) (apply #'imref arr idx)) )))
      (%flatten nil (reverse (ima-dims arr)))
      new-arr )))

