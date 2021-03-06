
(in-package :ima)

;; @\section{Lists}

;; @{\em Warning:} Lists of lists are intepreted as higher dimensionality IMAs.
;; To avoid most nonsense, we will just say right now, having lists as elements
;; is undefined.  Rather it is well defined what will happen, but it is very
;; easy to get tricked up if you plan to hold lists in you list based IMAs.  So
;; don't do it unless you are very sure of what you are doing.  Examples:

;; @For that matter, ``ragged'' arrays should also be avoided.

;; @Here we have our nested list interface for IMA.  Understand that it is
;; provided here because lists are so common, but this is far from an efficient
;; data structure for an array (think random access in linear time, ugh).  In
;; light of this, I didn't really try to make this the best it can be, so it is
;; probably very slow, so don't use it for calculations.  One thing I tried very
;; hard to do is to make it easy to convert between underlying data structures,
;; so convert to something else!

;; @That being said, it was a surprise to me when I found out that you might not
;; notice the linear time versus constant time difference.  If you had an IMA of
;; ~50 elements, you might expect random access on that IMA as a list to be ~25
;; times slower than on the array.  In reality (SBCL), it is around 25% percent
;; slower, or 1.25 times slower, a far cry from 25.  Why is this?  I don't know.
;; Perhaps SBCL is very good at compiling this down to efficient code.  More
;; likely, <<imref>> needs optimizing (doing this with <<aref>> runs about 10
;; times faster).

;; @Dimensionality is also a weak spot when it comes to list IMAs.  This will
;; usually work as expected, but there are some corner cases where things just
;; won't.  For, instance, you are very limited in your ability to make an array
;; that has no elements in it.  For this kind of work, I suggest that you use a
;; Lisp array, which will behave identically when used as an IMA or array.

;;<<ima-dimension-lists,2>>=
(defmethod ima-dimension ((ima cons) axis)
  (length (n-times axis #'car ima)))

(defmethod ima-dimension ((ima (eql nil)) axis)
  0)

;;<<ima-dimensions-lists,2>>=
(defmethod ima-dimensions ((ima cons))
  ;; This extra binding makes IMA work on CMUCL
  (let ((ima ima))
    (iter (while (consp ima))
      (collect (length ima))
      (setf ima (car ima)))))

(defmethod ima-dimensions ((ima (eql nil)))
  (list 0))


;;<<>>=
(defmethod imref ((ima cons) &rest idx)
  (declare (optimize (speed 3) (debug 1) (compilation-speed 0) (safety 1) (space 0))
           (dynamic-extent idx))
  (cond ((null (cdr idx)) (elt ima (first idx)))
        (t (apply #'imref (elt ima (first idx))
                  (rest idx)))))

;;<<>>=
(defmethod (setf imref) (val (ima cons) &rest idx)
  (labels ((set-spot (list idx)
             (if (null (cdr idx))
                 (setf (nth (car idx) list) val)
                 (set-spot (nth (car idx) list) (cdr idx)))))
    (set-spot ima idx)))

;;<<>>=
(defun group (source n)
  (cond ((null source) source)
        (t
         (if (zerop n) (error "zero length"))
         (labels ((rec (source acc)
                    (let ((rest (nthcdr n source)))
                      (if (consp rest)
                          (rec rest (cons (subseq source 0 n) acc))
                          (nreverse (cons source acc))))))
           (if source (rec source nil) nil)))))

;;<<>>=
(def-unmapper list (ima)
  (labels ((builder (list extents)
             (if extents
                 (builder (group list (car extents)) (cdr extents))
                 list)))
    (builder (iter (for i below (apply #'* (ima-dimensions ima)))
                   (collect (apply #'imref ima (nd-index i (ima-dimensions ima)))))
             (butlast (reverse (ima-dimensions ima))))))

;;<<>>=
(defun make-list-array (extent)
  (if (null (cdr extent))
      (make-list (car extent) :initial-element 0)
      (iter (for i below (car extent))
            (collect (make-list-array (cdr extent))))))

;;<<>>=
(defmethod make-ima-like ((list list) &key (dims (ima-dimensions list))
                                           &allow-other-keys)
  (make-list-array dims))

;; (defmethod make-ima ((like-this list)
;;                      &key ima-dimensions data-format
;;                      (contents nil contents-p))
;;   (declare (ignore data-format contents contents-p))
;;   (make-list-array (aif ima-dimensions it (ima-dimensions like-this))))

;; @In order to have list IMAs behave as we have decided they should
;; (i.e. mutations to any mapping of any array must show up in the other
;; mappings), we can't do things like set a row of the matrix to a list in the
;; straightforward way we might like.  We instead have to run of the elements
;; and set them ourselves.

;; @Okay, I actually don't understand how or why the following works, but it
;; does.  I will just put some stuff in the test suite to make sure it actually
;; does work, and works everywhere.  Here we are doing what I said we couldn't
;; do just above.

;;<<>>=
(defmethod get-vector ((ima cons) n &rest fixed)
  (let ((row-direction (1- (length (ima-dimensions ima)))))
    (if (= n row-direction)
        (apply #'imref ima fixed)
        (call-next-method))))

;;<<>>=
(defmethod (setf get-vector) ((new-val cons) (ima cons) n &rest fixed)
  (let ((row-direction (1- (length (ima-dimensions ima)))))
    (if (= n row-direction)
        (apply #'(setf imref) new-val ima fixed)
        (call-next-method))))

;;<<>>=
(defmethod get-block ((ima cons) start extent)
  (let ((dims (ima-dimensions ima))
        (first-offset (first start)))
    (if (= 0 first-offset)
        (if (and (= (first extent) (length ima))
                 (every (/. (s e d) (and (= 0 s) (= e d)))
                        (cdr start) (cdr dims) (cdr extent)))
            ima
            (call-next-method))
        (get-block (nthcdr first-offset ima) (cons 0 (rest start)) extent))))

;;<<>>=
(defmethod (setf get-block) ((new-val cons) (ima cons) start extent)
  (let ((dims (ima-dimensions ima))
        (first-offset (first start)))
    (if (and (= (first extent) (length ima))
             (every (/. (s e d) (and (= 0 s) (= e d)))
                    (cdr start) (cdr dims) (cdr extent)))
        (setf (nth first-offset ima) new-val)
        (call-next-method))))

