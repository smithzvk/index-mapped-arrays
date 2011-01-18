
;; gsl.lisp - GSL matrix vector interfaces for IMA

(defpackage :ima-gsl
    (:use :ima :cl :iterate :toolbox :gsl-bindings)
  (:shadow #:gsl-matrix #:gsl-vector)
  (:export
   ;; Wrapping functions
   #:wrap-copied-gsl-vector-w/gc
   #:wrap-copied-gsl-matrix-w/gc
   #:wrap-gsl-vector-w/gc
   #:wrap-gsl-matrix-w/gc
   #:wrap-gsl-vector
   #:wrap-gsl-matrix
   #:gsl-matrix #:gsl-vector #:gsl-array
   #:make-gsl-vector #:make-gsl-matrix #:make-gsl-array
   ;; IMA interface
   #:imref #:ima-dimension #:ima-dimensions
   #:backend-of #:map-of #:data-of #:index-mapped-array
   #:get-vector #:column-vector #:row-vector
   #:get-diagonal
   #:get-projection
   #:get-block #:submatrix
   #:transpose
   #:unmap #:unmap-into #:def-unmapper ))

(in-package :ima-gsl)

;; @\section{Wrappers for GSL vectors and matricies}


;;; Some basic wrapper stuff to make GSL arrays more bearable

(defclass gsl-array ()
  ((data :initarg :data :accessor raw :accessor data-of); :type sb-sys:system-area-pointer)
   (raw-data :initarg :raw-data :accessor raw-data-of); :type sb-sys:system-area-pointer)
   (dims :initarg :dims :accessor dims-of :type list)
   (parents :initarg :parents :accessor parents-of) ))

;; (defstruct sgsl-vector
;;   (array (cffi:null-pointer) :type sb-sys:system-area-pointer)
;;   (dims '() :type list)
;;   (parents nil)
;;   (data  (cffi:null-pointer) :type sb-sys:system-area-pointer)
;;   (stride 1 :type fixnum) )

;; (defun create-sgsl-vector (&rest args)
;;   (let ((arr (apply #'make-gsl-vector args)))
;;     (make-sgsl-vector :array (data-of arr)
;;                       :dims (dims-of arr)
;;                       :parents arr
;;                       :data (gsl-vector-data (data-of arr))
;;                       :stride (gsl-vector-stride (data-of arr)) )))

;; (defun simref (ima idx)
;;   (declare (optimize (speed 3) (debug 0) (safety 0))
;;            (inline simref)
;;            (type sgsl-vector ima)
;;            (type fixnum idx)
;;            (ftype (function (* fixnum) double-float) imref) )
;;   (if (> (the fixnum (car (sgsl-vector-dims ima))) (the fixnum idx))
;;       (cffi:mem-aref (sgsl-vector-data ima) :double
;;                      (the fixnum (* (the fixnum idx)
;;                                     (the fixnum (sgsl-vector-stride ima)) )))
;;       (error "~a out of bounds of dimensions ~a" idx (sgsl-vector-dims ima)) ))

(defclass gsl-matrix (gsl-array) ())
;;  ((tda :initarg :tda :accessor tda-of)) )

(defclass gsl-vector (gsl-array) ())
;;   ((stride :initarg :stride :accessor stride-of)) )

(defun wrap-copied-gsl-vector-w/gc (raw-vector)
  (let ((new (vector-alloc (gsl-vector-size raw-vector))))
    (vector-memcpy new raw-vector)
    (tg:finalize
     (make-instance 'gsl-vector
                    :data new
                    :dims (list (gsl-vector-size raw-vector)) )
     (/. () (gsl-bindings:vector-free new)) )))

(defun wrap-copied-gsl-matrix-w/gc (raw-matrix)
  (let ((new (matrix-alloc (gsl-matrix-size1 raw-matrix)
                           (gsl-matrix-size2 raw-matrix) )))
    (matrix-memcpy new raw-matrix)
    (tg:finalize
     (make-instance 'gsl-matrix
                    :data raw-matrix
                    :dims (list (gsl-matrix-size1 raw-matrix)
                                (gsl-matrix-size2 raw-matrix) ))
     (/. () (gsl-bindings:matrix-free new)) )))

(defun wrap-gsl-vector-w/gc (raw-vector)
  (tg:finalize
   (make-instance 'gsl-vector
                  :data raw-vector
                  :dims (list (gsl-vector-size raw-vector)) )
   (/. () (gsl-bindings:vector-free raw-vector)) ))

(defun wrap-gsl-matrix-w/gc (raw-matrix)
  (tg:finalize
   (make-instance 'gsl-matrix
                  :data raw-matrix
                  :dims (list (gsl-matrix-size1 raw-matrix)
                              (gsl-matrix-size2 raw-matrix) ))
   (/. () (gsl-bindings:matrix-free raw-matrix)) ))

(defun wrap-gsl-vector (raw-vector)
  (make-instance 'gsl-vector
                 :data raw-vector
                 :dims (list (gsl-vector-size raw-vector)) ))

(defun wrap-gsl-matrix (raw-matrix)
  (make-instance 'gsl-matrix
                 :data raw-matrix
                 :dims (list (gsl-matrix-size1 raw-matrix)
                             (gsl-matrix-size2 raw-matrix) )))

;;; Are these used for anything??
;; (defctype gsl-matrix (:wrapper :pointer
;;                                :from-c wrap-gsl-matrix-w/gc
;;                                :to-c ima::data-of ))

;; (defctype gsl-matrix-reference (:wrapper :pointer
;;                                          :from-c wrap-gsl-matrix
;;                                          :to-c ima::data-of ))

;; (defctype gsl-vector (:wrapper :pointer
;;                                :from-c wrap-gsl-matrix-w/gc
;;                                :to-c ima::data-of ))

;; (defctype gsl-vector-reference (:wrapper :pointer
;;                                          :from-c wrap-gsl-matrix
;;                                          :to-c ima::data-of ))

;; (defvar *image-storable-data* nil)

;; (defun store-foreign-data ()
;;   (with-open-file (str #p"tmp.object-store" :direction :output)
;;     (iter (for (dat store-fn restore-fn) in *image-storable-data*)
;;           (funcall store-fn dat str) )))

;; (push #'store-foreign-data sb-ext:*save-hooks*)

(defun make-gsl-vector (num-elements &key
                        initial-element
                        initial-contents )
  (let* ((raw-gsl-vector
          (cond (initial-contents
                 (let ((raw-vector (vector-alloc num-elements)))
                   (iter (for i below num-elements)
                         (for val in initial-contents)
                         (vector-set raw-vector i val) )
                   raw-vector ))
                ((and initial-element (/= initial-element 0))
                 (let ((raw-vector (vector-alloc num-elements)))
                   (iter (for i below num-elements)
                         (vector-set raw-vector i initial-element) )
                   raw-vector ))
                (t (vector-calloc num-elements)) ))
         (polished-vector (wrap-gsl-vector raw-gsl-vector)) )
    (tg:finalize polished-vector (/. () (vector-free raw-gsl-vector)))
    polished-vector ))

(defun make-gsl-matrix (dimensions &key
                        initial-element
                        initial-contents )
  (when (member 0 dimensions) (error "Matricies cannot have an extent of 0 (zero)"))
  (let* ((raw-gsl-matrix
          (cond (initial-contents
                 (let ((raw-matrix
                        (apply #'matrix-alloc dimensions) ))
                   (if (eql :identity initial-contents)
                       (matrix-set-identity raw-matrix)
                       (iter (for i below (car dimensions))
                             (for row in initial-contents)
                             (iter (for j below (cadr dimensions))
                                   (for val in row)
                                   (matrix-set raw-matrix i j val) )))
                   raw-matrix ))
                ((and initial-element (/= initial-element 0))
                 (let ((raw-matrix
                        (apply #'matrix-alloc dimensions) ))
                   (matrix-set-all raw-matrix initial-element)
                   raw-matrix ))
                (t (apply #'matrix-calloc dimensions))) )
         (polished-matrix (wrap-gsl-matrix raw-gsl-matrix)) )
    (tg:finalize polished-matrix (/. () (matrix-free raw-gsl-matrix)))
    polished-matrix ))

(defun make-gsl-array (dimensions &key
                       initial-element
                       initial-contents )
  (let ((dimensions (if (consp dimensions) dimensions (list dimensions))))
    (case (length dimensions)
      (1 (make-gsl-vector (car dimensions)
                          :initial-element initial-element
                          :initial-contents initial-contents ))
      (2 (make-gsl-matrix dimensions
                          :initial-element initial-element
                          :initial-contents initial-contents ))
      (otherwise (error "GSL arrays are either vectors or matricies")) )))

;; @\section{GSL extensions}

(defmethod ima-dimension ((ima gsl-array) axis)
  (nth axis (ima-dimensions ima)) )
(defmethod ima-dimensions ((ima gsl-array))
  (dims-of ima) )

(defun out-of-bounds (dim idx)
  (cond ((null dim) nil)
        ((<= (car dim) (car idx)) t)
        (t (out-of-bounds (cdr dim) (cdr idx))) ))

(defmethod imref ((mat gsl-matrix) &rest idx)
  (declare (optimize (speed 3) (debug 0) (safety 1))
           (dynamic-extent idx) )
  (destructuring-bind (i j) idx
    (destructuring-bind (n m) (dims-of mat)
      (declare (type fixnum i j n m))
      (if (and (> n i -1) (> m j -1))
          (let ((data (data-of mat)))
            (cffi:mem-aref (gsl-matrix-data data) :double
                           (the fixnum (+ (the fixnum (* i (the fixnum (gsl-matrix-tda data)))) j)) ))
          (error "~a out of bounds of dimensions ~a" idx (dims-of mat)) ))))
(defmethod (setf imref) (val (mat gsl-matrix) &rest idx)
  (declare (optimize (speed 3) (debug 0) (safety 1))
           (dynamic-extent idx) )
  (destructuring-bind (i j) idx
    (declare (type fixnum i j))
    (destructuring-bind (n m) (dims-of mat)
      (declare (type fixnum n m))
      (if (and (> n i -1) (> m j -1))
          (let ((data (data-of mat)))
            (setf (cffi:mem-aref (gsl-matrix-data data) :double
                                 (the fixnum (+ (the fixnum (* i (the fixnum (gsl-matrix-tda data)))) j)) )
                  (float val 0d0) ))
          (error "~a out of bounds of dimensions ~a" idx (dims-of mat)) ))))

(defmethod imref ((vec gsl-vector) &rest idx)
  (declare (optimize (speed 3) (debug 0))
           (dynamic-extent idx) )
  (if (> (the fixnum (car (slot-value vec 'dims))) (the fixnum (car idx)) -1)
      (let ((data (slot-value vec 'data)))
        (cffi:mem-aref (gsl-vector-data data) :double
                       (the fixnum (* (the fixnum (car idx))
                                      (the fixnum (gsl-vector-stride data)) ))))
      (error "~a out of bounds of dimensions ~a" idx (dims-of vec)) ))
(defmethod (setf imref) (val (vec gsl-vector) &rest idx)
  (declare (optimize (speed 3) (debug 0))
           (dynamic-extent idx) )
  (if (> (the fixnum (car (slot-value vec 'dims))) (the fixnum (car idx)) -1)
      (let ((data (slot-value vec 'data)))
        (setf (cffi:mem-aref (gsl-vector-data data) :double
                             (the fixnum (* (the fixnum (car idx))
                                            (the fixnum (gsl-vector-stride data)) )))
              (float val 0d0) ))
      (error "~a out of bounds of dimensions ~a" idx (dims-of vec)) ))

(def-unmapper gsl-array (ima)
  (cond ((= 1 (length (ima-dimensions ima)))
         (let* ((arr (make-gsl-vector (car (ima-dimensions ima)))))
           (iter (for i below (apply #'* (ima-dimensions ima)))
                 (apply #'gsl-bindings::vector-set
                        (raw arr)
                        (append (nd-index i (ima-dimensions ima))
                                (list
                                 (apply #'imref ima
                                        (nd-index i (ima-dimensions ima)) )))))
           arr ))
        ((= 2 (length (ima-dimensions ima)))
         (let* ((arr (make-gsl-matrix (ima-dimensions ima))))
           (iter (for i below (apply #'* (ima-dimensions ima)))
                 (apply #'gsl-bindings::matrix-set
                        (raw arr)
                        (append (nd-index i (ima-dimensions ima))
                                (list
                                 (apply #'imref ima
                                        (nd-index i (ima-dimensions ima)) )))))
           arr ))
        (t (error "This cannot be mapped onto a GSL matrix or vector")) ))

(ima::def-maker gsl-array (dims)
  (make-gsl-array dims) )

(defun matrix-row (mat index)
  (let* ((raw-vec (cffi:foreign-alloc 'gsl-bindings::gsl-vector))
         (vec (make-instance 'gsl-vector :data raw-vec
                             :dims (list (ima-dimension mat 1))
                             :parents (list mat) )))
    (tg:finalize vec (/. () (cffi:foreign-free raw-vec)))
    (setf (gsl-vector-data raw-vec)
          (cffi:inc-pointer
           (gsl-matrix-data (raw mat))
           (* (gsl-matrix-tda (raw mat)) index
              (cffi:foreign-type-size :double) )))
    (setf (gsl-vector-block raw-vec)
          (gsl-matrix-block (raw mat)) )
    (setf (gsl-vector-size raw-vec)
          (gsl-matrix-size2 (raw mat)) )
    (setf (gsl-vector-stride raw-vec) 1)
    (setf (gsl-vector-owner raw-vec) 0)
    vec ))

(defun matrix-col (mat index)
  (let* ((raw-vec (cffi:foreign-alloc 'gsl-bindings::gsl-vector))
         (vec (make-instance 'gsl-vector :data raw-vec
                             :dims (list (ima-dimension mat 0))
                             :parents (list mat) )))
    (tg:finalize vec (/. () (cffi:foreign-free raw-vec)))
    (setf (gsl-vector-data raw-vec)
          (cffi:inc-pointer
           (gsl-matrix-data (raw mat))
           (* index (cffi:foreign-type-size :double)) ))
    (setf (gsl-vector-block raw-vec)
          (gsl-matrix-block (raw mat)) )
    (setf (gsl-vector-size raw-vec)
          (gsl-matrix-size1 (raw mat)) )
    (setf (gsl-vector-stride raw-vec)
          (gsl-matrix-tda (raw mat)) )
    (setf (gsl-vector-owner raw-vec) 0)
    vec ))

(defmethod get-vector ((ima gsl-matrix) n &rest fixed)
  (case n
    (1 (matrix-row ima (first fixed)))
    (0 (matrix-col ima (first fixed)))
    (otherwise (error "~A higher than the dimensionality of ~A" n ima)) ))

(defmethod get-diagonal ((ima gsl-matrix))
  (let* ((raw-vec (cffi:foreign-alloc 'gsl-bindings::gsl-vector))
         (vec (make-instance 'gsl-vector :data raw-vec
                             :dims (list (ima-dimension ima 0))
                             :parents (list ima) )))
    (tg:finalize vec (/. () (cffi:foreign-free raw-vec)))
    (setf (gsl-vector-data raw-vec)
          (gsl-matrix-data (raw ima)) )
    (setf (gsl-vector-block raw-vec)
          (gsl-matrix-block (raw ima)) )
    (setf (gsl-vector-size raw-vec)
          (gsl-matrix-size1 (raw ima)) )
    (setf (gsl-vector-stride raw-vec)
          (1+ (gsl-matrix-tda (raw ima))) )
    (setf (gsl-vector-owner raw-vec) 0)
    vec ))

(defmethod get-diagonal ((ima gsl-vector))
  ima )

(defmethod get-projection ((ima gsl-matrix) n val)
  (get-vector ima (mod (1+ n) 2) val) )

(defmethod get-block ((ima gsl-matrix) start extent)
  (let* ((raw-mat (cffi:foreign-alloc 'gsl-bindings::gsl-matrix))
         (mat (make-instance 'gsl-matrix :data raw-mat
                             :dims extent
                             :parents (list ima) )))
    (tg:finalize mat (/. () (cffi:foreign-free raw-mat)))
    (destructuring-bind (size1 size2) extent
      (destructuring-bind (start1 start2) start
        (setf (gsl-matrix-data raw-mat)
              (cffi:inc-pointer
               (gsl-matrix-data (raw ima))
               (* (cffi:foreign-type-size :double)
                  (+ (* start1 (gsl-matrix-tda (raw ima)))
                     start2 ))))
        (setf (gsl-matrix-block raw-mat)
              (gsl-matrix-block (raw ima)) )
        (setf (gsl-matrix-size1 raw-mat)
              size1 )
        (setf (gsl-matrix-size2 raw-mat)
              size2 )
        (setf (gsl-matrix-tda raw-mat)
              (gsl-matrix-tda (raw ima)) )
        (setf (gsl-matrix-owner raw-mat) 0)
        mat ))))

(defmethod get-block ((ima gsl-vector) start extent)
  (let* ((raw-vec (cffi:foreign-alloc 'gsl-bindings::gsl-vector))
         (vec (make-instance 'gsl-vector :data raw-vec
                             :dims extent
                             :parents (list ima) )))
    (tg:finalize vec (/. () (cffi:foreign-free raw-vec)))
    (setf (gsl-vector-data raw-vec)
          (cffi:inc-pointer
           (gsl-vector-data (raw ima))
           (* (first start) (gsl-vector-stride (raw ima))
              (cffi:foreign-type-size :double) )))
    (setf (gsl-vector-block raw-vec)
          (gsl-vector-block (raw ima)) )
    (setf (gsl-vector-size raw-vec)
          (first extent) )
    (setf (gsl-vector-stride raw-vec)
          (gsl-vector-stride (raw ima)) )
    (setf (gsl-vector-owner raw-vec) 0)
    vec ))

(defmethod print-object ((mat gsl-matrix) str)
  (format str "#2G")
  (destructuring-bind (i-max j-max) (dims-of mat)
    (pprint-logical-block (str nil :prefix "(" :suffix ")")
      (dotimes (i i-max)
        (pprint-logical-block (str nil :prefix "(" :suffix ")")
          (dotimes (j j-max)
            (when (not (= j 0)) (write-char #\Space str))
            (format str "~A" (imref mat i j)) ))
        (when (not (= i (1- i-max)))
          (write-char #\Space str)
          (pprint-newline :linear str) )))))

(defmethod print-object ((vec gsl-vector) str)
  (format str "#G")
  (let ((max (ima-dimension vec 0)))
    (pprint-logical-block (str nil :prefix "(" :suffix ")")
      (dotimes (i max)
        (when (not (= i 0)) (write-char #\Space str))
        (pprint-newline :fill str)
        (format str "~A" (imref vec i)) ))))

(defun gsl-vec-mat-reader (stream subchar arg)
  (declare (ignore subchar))
  (let ((data (read stream)))
    (case arg
      ((1 nil) (make-gsl-vector (length data) :initial-contents data))
      (2 (make-gsl-matrix (ima-dimensions data) :initial-contents data))
      (otherwise (error "Invalid dimensionality")) )))

(set-dispatch-macro-character #\# #\G #'gsl-vec-mat-reader)


;; @<<imref2>> is no faster than doing the CLOS method invocation.  I
;; guess the only way to speed things up is to either improve the CLOS
;; imp optimizations, or write my one source level optimizer.  The
;; source level optimizer is portable, so maybe that is the correct
;; route.

;;<<>>=
(defun imref2 (ima &rest idx)
  (cond ((arrayp ima)
         (apply #'aref ima idx) )
        ((typep ima 'gsl-vector)
         (if (> (the fixnum (car (slot-value ima 'dims))) (the fixnum (car idx)))
             (let ((data (slot-value ima 'data)))
               (cffi:mem-aref (gsl-vector-data data) :double
                              (the fixnum (* (the fixnum (car idx))
                                             (the fixnum (gsl-vector-stride data)) ))))
             (error "~a out of bounds of dimensions ~a" idx (dims-of ima)) ))
        ((typep ima 'gsl-matrix)
         (destructuring-bind (i j) idx
           (declare (type fixnum i j))
           (let ((data (data-of ima)))
             (cffi:mem-aref (gsl-matrix-data data) :double
                            (the fixnum (+ (the fixnum (* i (the fixnum (gsl-matrix-tda data)))) j)) ))))))


