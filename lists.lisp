
(in-package :ima)

;; @\subsection{Lists}

;; @{\em Warning:} Lists of lists are intepreted as higher
;; dimensionality IMAs.  To avoid most nonsense, we will just say
;; right now, having lists as elements is undefined.  Rather it is
;; well defined what will happen, but it is very easy to get tricked
;; up if you plan to hold lists in you list based IMAs.  So don't do
;; it unless you are very sure of what you are doing.  Examples:

;; @For that matter, ``ragged'' arrays should also be avoided.

;; @Here we have our nested list interface for IMA.  Understand that
;; it is provided here because lists are so common, but this is far
;; from an efficient data structure for an array (think random access
;; in linear time, ugh).  I light of this, I didn't really try to make
;; this the best it can be, so it is probably very slow, so don't use
;; it for calculations.  One thing I tried very hard to do is to make
;; it easy to convert between underlying data structures, so convert
;; to something else!

;; @That being said, it was a surprise to me when I found out that you
;; might not notice the linear time versus constant time difference.
;; If you had an IMA of ~50 elements, you might expect random access
;; on that IMA as a list to be ~25 times slower than on the array.  In
;; reality (SBCL), it is around 25% percent slower, or 1.25 times
;; slower, a far cry from 25.  Why is this?  I don't know.  Perhaps
;; SBCL is very good at compiling this down to efficient code.  More
;; likely, <<imref>> needs optimizing (doing this with <<aref>> runs
;; about 10 times faster).

(defmethod ima-dimension ((ima cons) axis)
  (length (n-times axis #'car ima)) )
(defmethod ima-dimensions ((ima cons))
  (iter (while (consp ima))
        (collect (length ima))
        (setf ima (car ima)) ))
(defmethod imref ((ima cons) &rest idx)
  (declare (optimize (speed 3) (debug 1) (compilation-speed 0) (safety 1) (space 0))
           (dynamic-extent idx) )
  (cond ((null (cdr idx)) (elt ima (first idx)))
        (t (apply #'imref (elt ima (first idx))
                  (rest idx) ))))
(defmethod (setf imref) (val (ima cons) &rest idx)
  (labels ((set-spot (list idx)
             (if (null (cdr idx))
                 (setf (nth (car idx) list) val)
                 (set-spot (nth (car idx) list) (cdr idx)) )))
    (set-spot ima idx) ))

(defmethod immod (val (ima list) &rest idx)
  (if (and (null (cdr idx)) (eql (car idx) 0))
      (cons val (cdr ima))
      (let ((new (copy-ima ima)))
        (setf (apply #'imref new idx) val)
        new )))

(def-unmapper list (ima)
  (labels ((builder (list extents)
             (if extents
                 (builder (group list (car extents)) (cdr extents))
                 list )))
    (builder (iter (for i below (apply #'* (ima-dimensions ima)))
                   (collect (apply #'imref ima (nd-index i (ima-dimensions ima)))) )
             (butlast (reverse (ima-dimensions ima))) )))

(defun make-list-array (extent)
  (if (null (cdr extent))
      (make-list (car extent) :initial-element 0)
      (iter (for i below (car extent))
            (collect (make-list-array (cdr extent))) )))

(def-maker list (dims)
  (make-list-array dims) )

;; (defmethod make-ima ((like-this list)
;;                      &key ima-dimensions data-format
;;                      (contents nil contents-p) )
;;   (declare (ignore data-format contents contents-p))
;;   (make-list-array (aif ima-dimensions it (ima-dimensions like-this))) )

(defmethod get-vector ((ima cons) n &rest fixed)
  (let ((row-direction (1- (length (ima-dimensions ima)))))
    (if (= n row-direction)
        (apply #'imref ima fixed)
        (call-next-method) )))

(defmethod get-block ((ima cons) start extent)
  (let ((first-offset (first start)))
    (if (= 0 first-offset)
        (call-next-method)
        (call-next-method (nthcdr first-offset ima) (cons 0 (rest start)) extent) )))

