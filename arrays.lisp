
(in-package :ima)

;; @\section{Arrays}

(defmethod ima-dimension ((ima array) axis)
  (array-dimension ima axis))
(defmethod ima-dimensions ((ima array))
  (array-dimensions ima))
(defmethod imref ((ima array) &rest idx)
  (declare (optimize (speed 3) (debug 1) (compilation-speed 0) (safety 1) (space 0))
           (dynamic-extent idx)
           (ftype (function (array &rest cons) t) imref)
           (inline imref))
  (apply #'aref ima idx))
(defmethod (setf imref) (val (ima array) &rest idx)
  (setf (apply #'aref ima idx) val))

(define-modf-method imref 1 (val (ima array) &rest idx)
  (let ((new (copy-array ima)))
    (setf (apply #'aref new idx) val)
    new))

(def-unmapper array (ima)
  (let* ((arr (make-array (ima-dimensions ima))))
    (if (= 1 (length (ima-dimensions ima)))
        ;; vector, treat it specially
        (iter (for i below (ima-dimension ima 0))
              (setf (aref arr i) (imref ima i)))
        ;; an array of 2 or more dimensions
        (iter (for i below (apply #'* (ima-dimensions ima)))
              (setf (row-major-aref arr i)
                    (apply #'imref ima (nd-index i (ima-dimensions ima))))))
    arr))

(defmethod make-ima-like ((ima array) &key (dims (ima-dimensions ima))
                                           (element-type (array-element-type ima)))
  (make-array dims :element-type element-type))

(defmethod get-vector ((ima array) n &rest fixed)
  (let ((row-direction (1- (length (array-dimensions ima)))))
    (if (= n row-direction)
        (make-array (array-dimension ima row-direction)
                    :displaced-to ima
                    :displaced-index-offset (apply #'array-row-major-index
                                                   ima (append fixed (list 0))))
        (call-next-method))))

(defmethod get-block ((ima vector) start extent)
  (fsubvec ima (first start) (+ (first start) (first extent))))

;; Modf methods

(define-modf-method get-vector 1 (new-val (ima array) n &rest fixed)
  (let* ((new-array (copy-array ima))
         (vec (apply #'get-vector new-array n fixed)))
    (iter (for el in-ima new-val with-index i)
      (setf (imref vec i) el))
    new-array))
