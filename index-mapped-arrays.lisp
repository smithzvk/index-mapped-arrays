
;; @\documentclass{article}

;; @\begin{document}

;; @\title{Index Mapped Array (or any data really)}

;; @\maketitle

;; @\begin{abstract}

;; Index mapped arrays or IMA is a well integrated, general method of dealing
;; with arrays in Common Lisp.  It provides facilities to perform common
;; mappings on lisp data, and foreign data, and provides a simple interface to
;; extend this behavior.  It is based around CLOS methods and extensions are
;; written as methods specializing on your data type.  IMA tries its best to be
;; more than a simple wrapper around a data type and will operate on the raw
;; type whenever possible, and falls back on an index mapping mechanism as
;; versatile as Common Lisp itself.

;; @\end{abstract}

(in-package :ima)

;; @\section{Introduction}

;; @\subsection{A word on the general structure}

;; As stated, IMA is built on CLOS.  I use it in some unorthodox ways, however.
;; The basic idea is that any object whose data can be mapped onto a sequence of
;; numbers can be thought of as an IMA.  That means that arrays, lists, strings,
;; hash tables, even structures and classes, can be thought of as IMAs.  So in
;; some sense, the class <<index-mapped-array>> should be thought of as a
;; superclass to almost anything; somewhere up there by T.  Since I didn't want
;; to edit the class hierarchy (and doing so is not portable, right?), many of
;; the IMA methods, <<ima-dimensions>>, <<get-vector>>, any map really,
;; specialize on class T.  These are keeping with the normal CLOS way, if you
;; want to modify the behavior define a more specific method and (possibly)
;; hijack the method chain.

;; There is another way that I am using CLOS.  I also wish to have methods that
;; search from least specific to most specific.  This could be implemented as a
;; method combination, but it is beyond my ability.  So as it is, I specialize
;; on class <<index-mapped-array>>, and that calls the method of the underlying
;; data type.  Methods that behave this way are <<imref>>, <<unmap-into>>, and
;; <<make-ima-like>>.

;; @\subsection{Building your own IMA}

;; @One of the best features of IMA is that it is easy to define your own IMA
;; interface to some data that the implementors have never even thought of.  You
;; just have to define methods for the interfaces you want to use.  A full IMA
;; interface for any data requires you to define <<ima-dimensions>>,
;; <<ima-dimension>>, <<imref>>, <<(setf imref)>> if you want to set elements,
;; Modf functions (which I'll refer to as <<(modf imref)>>) if you want to
;; provide a functional modification method, and a set of <<unmap-into>> methods
;; and <make-ima-like>> methods.  It sounds like a lot, but it really isn't.
;; Some methods can be omitted with decreased functionality.

;; @The functions <<ima-dimension>>, <<ima-dimensions>>, and <<imref>> are
;; necessary to hook your data structure into the generic interface.  They are
;; absolutely required.

;; @The functions <<(setf imref)>> and <<(modf imref)>> offer a way to mutate or
;; functionally change the IMA.  Typically one is used more often than the other
;; because a data structure either aims to be functional or mutable.  If you are
;; using a mutable data format, it might be nice if you also include an <<(modf
;; imref)>> definition, but if you using an immutable data structure, it is
;; often impossible to provide a {\em setf} that behaves sanely.

;; @The <<unmap-into>> functions allow the user to recieve a version of the
;; array that is element by element equal (in some sense of the word) to the
;; IMA.  This might entail no work at all, i.e. you are not performing any
;; emulated index mappings, or it may entail a complete copying of the array.
;; It also allows for transforming between possible IMA representations, such as
;; from an array to nested lists or a GSL matrix.  Since this can return you the
;; same instance, it is helpful to have a <<make-ima-like>> method which
;; gaurantees that the memory will be freshly allocated (but the contents not
;; copied, for copying, see <<copy-ima>>).

;;; Index mapping

;; @\section{The IMA class and index mapping}

;;<<>>=
(defclass index-mapped-array ()
  ((data :initarg :data :accessor data-of)
   (dims :initarg :dims :accessor dims-of)
   (map-desc :initarg :map-desc :accessor map-desc-of)
   (map :initarg :map :accessor map-of)))

(defmethod map-desc-of (ima) :raw)

(locally
    (declare (optimize (speed 3)))
  (defmethod ima-dimension (ima axis)
    "Return the length of IMA along AXIS."
    (nth axis (ima-dimensions ima)))
  (defmethod ima-dimensions (ima)
    "Return the extents of the IMA."
    (dims-of ima))
  (defmethod imref ((ima index-mapped-array) &rest idx)
    "Get the element of IMA at indices IDX."
    (declare (optimize (speed 3) (debug 1) (compilation-speed 0) (safety 1) (space 0))
             (dynamic-extent idx))
    (apply #'imref (data-of ima) (funcall (the function (map-of ima)) idx)))
  (defmethod (setf imref) (val (ima index-mapped-array) &rest idx)
    "Set the value of IMA at indices IDX to value VAL."
    (setf (apply #'imref (data-of ima)
                 (funcall (the function (map-of ima)) idx))
          val)))

;; @{\em Note:} If you are using your IMA in a functional way with <<(modf
;; imref)>>, you should probably not be using <<(setf imref)>>.  <<(setf
;; imref)>> changes the original data, <<(modf imref)>> returns a new structure
;; and makes no promises on whether structure is shared or not.  You could
;; imagine that you might get something back from <<(modf imref)>> that shares
;; part of it's structure with the underlying data.  Then when you set, it might
;; be changing the original data, or not.  When you look at that orignial array,
;; it is very likely that it has been corrupted.

;; The function <<(modf imref)>> is the analog of the function <<(setf imref)>>.
;; It returns a new IMA with the given, single element changes to a new value.
;; Using Modf, we can define methods that modify general map places, like
;; <<column-vector>> for instance.

(define-modf-method imref 1 (val (ima index-mapped-array) &rest idx)
  "This is an index-mapped-array strcuture, so we will pass the work down to the
underlying structure."
  (map-indices (modf (apply #'imref (modf-eval (data-of ima))
                            (funcall (map-of ima) idx))
                     val)
               (map-of ima)
               (dims-of ima)
               :map-desc (map-desc-of ima)))

(defmethod ima-flat-ref (ima index &optional (dimensions (ima-dimensions ima)))
  "Allows you to access the data of an IMA in a linear fashion.  No guarantees
are made as to the order in which the elements are ordered \(this may change in
the future if it becomes benefitial)."
  (apply #'imref ima
         (nd-index index dimensions)))

(defmethod (setf ima-flat-ref) (val ima index
                                    &optional (dimensions (ima-dimensions ima)))
  "Allows you to set the data of an IMA by referencing the data in a linear
fashion."
  (setf (apply #'imref ima
               (nd-index index dimensions))
        val))

(defmethod flat-ima-size (ima)
  "Returns the linear size of an IMA."
  (apply #'* (ima-dimensions ima)))

(defvar *simplify* t)

(defun map-indices (object map dims &key (map-desc :unknown))
  "Create an object of type index-mapped-array.  This is basically a fall back
for times when you want a mapping that a data type can't do natively \(which is
quite often)."
  (or (and *simplify* (let ((*simplify* nil)) (simplify map-desc object)))
      (make-instance 'index-mapped-array
                     :data object
                     :map map
                     :dims dims
                     :map-desc (list map-desc (map-desc-of object)))))

;; @\section{Common (built in) maps}

;; @This is an ugly, fragile macro that declares generic imref and (setf imref)
;; and any nicer wrapper functions.  {\bf NOTE:} Function bodies must be
;; explicit, and must contain nothing but a call to the underlying mapping
;; method.  Some variable capture possibilties which need to be fixed.

;; @Here we define a generic way to create maps, a generic way to set the data
;; in the underlying data structure that is referenced by the map, and a generic
;; way to make a new instance of the IMA with given changes to the data
;; referenced by the map.  Each of these generic features are in essence
;; inefficient.  The mapping requires an extra function evaluation.  The generic
;; setf of a map requires access via imref, which is known to be slow.  The
;; generic modification of maps takes the cake and will require O(N*m) time
;; where N is the number of data elements and m is the number of desired changes
;; when applied to many things that require a full copy, for instance arrays.
;; This can be aleviated by speciallizing the modf method, which we do.

(defmacro def-generic-map ((defmethod name (ima &rest args) &body body)
                           &rest convenience-functions)
  "Define a generic map which includes an IMREF method definition and a \(SETF
IMREF) method definition."
  (declare (ignore defmethod))
  (with-gensyms (mapped-data-sym mapped-i-sym new-val-sym)
    `(progn
       (defmethod ,name (,ima ,@args)
         ,@body)
       (defmethod (setf ,name) (,new-val-sym ,ima ,@args)
         (let ((,mapped-data-sym ,(if (member '&rest args)
                                      (append (list 'apply
                                                    `(function ,name) ima)
                                              (remove '&rest args))
                                      (list* name ima args))))
           (iter (for dest-el in-ima ,mapped-data-sym with-index ,mapped-i-sym)
                 (for el in-ima ,new-val-sym)
                 (setf (apply #'imref ,mapped-data-sym
                              (nd-index ,mapped-i-sym
                                        (ima-dimensions ,mapped-data-sym)))
                       el))
           ,new-val-sym))
       (define-modf-method ,name 1 (,new-val-sym ,ima ,@args)
         (let* ((,ima (map-indices ,ima
                                   #'identity
                                   (ima-dimensions ,ima)))
                (,mapped-data-sym
                  ,(if (member '&rest args)
                       (append (list 'apply `(function ,name) ima)
                               (remove '&rest args))
                       (list* name ima args)))
                (ret ,ima))
           (iter (for dest-el in-ima ,mapped-data-sym with-index ,mapped-i-sym)
                 (for el in-ima ,new-val-sym)
             (setf ret
                   (modf (apply
                          #'imref ret
                          (funcall (map-of ,mapped-data-sym)
                                   (nd-index ,mapped-i-sym
                                             (ima-dimensions ,mapped-data-sym))))
                         el)))
           (data-of ret)))
       ,@(iter (for (defun conv-name conv-args . conv-body) in convenience-functions)
               (collecting
                (destructuring-bind (doc-string body)
                    (if (stringp (first conv-body))
                        (list (list (first conv-body)) (rest conv-body))
                        (list nil conv-body))
                  (cond ((= 1 (length body))
                         `(progn (defun ,conv-name ,conv-args ,@doc-string ,@body)
                                 (defun (setf ,conv-name) ,(cons new-val-sym
                                                            conv-args)
                                   ,@doc-string
                                   (setf ,@body ,new-val-sym))
                                 (define-modf-function ,conv-name 1
                                     ,(cons new-val-sym conv-args)
                                   ,@doc-string
                                   (modf ,@body ,new-val-sym))))
                        (t
                         (warn "Not creating a SETF or MODF function.  In order to create a SETF function, your convenience function bodies can only be a single form which is usable as a place.")
                         `(defun ,conv-name ,conv-args ,@doc-string ,@body)))))))))

;; @<<self-map>> is a trick to allow you to {\em setf} entire IMA contents.
;; {\em contents-of} is a more plain english desciptive name of the facility.

;;<<>>=
(def-generic-map
    (defmethod self-map (ima)
      "Return an identity map of the IMA.  Useful if you want to SETF an entire
array."
      ima)
    (defun contents-of (ima)
      "Return an identity map of the IMA.  Useful if you want to SETF an entire
array."
      (self-map ima)))

;; @\section{Index maps that reduce complexity}

;;<<>>=
(def-generic-map
    (defmethod get-slice (ima n val)
      "This reduces the dimensionality, D, to D-1."
      (map-indices ima (/. (idx) (append (subseq idx 0 n) (list val) (subseq idx n)))
                   (let ((count -1))
                     (remove-if (/. (_) (= n (incf count))) (ima-dimensions ima)))
                   :map-desc (list :slice n val))))

;;<<>>=
(def-generic-map
    (defmethod get-vector (ima n &rest fixed)
      "This reduces the dimensionality to 1, i.e. a vector."
      (map-indices ima (/. (idx)
                          (append (subseq fixed 0 n) idx (subseq fixed n)))
                   (list (ima-dimension ima n))
                   :map-desc (list :vec n fixed)))
    (defun column-vector (ima n)
      "Get the column vector of a 2-D array.  The last index is fixed, the index
of the vector changes the second index on the array."
      (get-vector ima 0 n))
    (defun row-vector (ima n)
      "Get the row vector of a 2-D array.  The first index is fixed, the index
of the vector changes the last index on the array."
      (get-vector ima 1 n)))

;;<<>>=
(def-generic-map
    (defmethod get-diagonal (ima)
      "Map to the vector representing the diagonal of a N-dimensional cubic
array."
      (map-indices ima (/. (idx)
                          (make-list
                           (length (ima-dimensions ima))
                           :initial-element (car idx)))
                   (list (ima-dimension ima 0))
                   :map-desc :diag)))

;;<<>>=
(def-generic-map
    (defmethod get-cross-diagonal (ima)
      "For 2-D arrays, return the opposite diagonal of the matrix, which crosses
the matrix diagonal."
      (unless (= (length (ima-dimensions ima)) 2)
        (error "The cross diagonal is only unique for 2D arrays \(matrices)"))
      (map-indices ima (/. (idx)
                          (let ((n (first (ima-dimensions ima))))
                            (list (- n (car idx) 1) (car idx))))
                   (list (ima-dimension ima 0)))))

;;<<>>=
(def-generic-map
    (defmethod get-block (ima start extent)
      "Get a sub-block of the array.  This does not change the dimensionality."
      (map-indices ima (/. (idx) (mapcar #'+ start idx)) extent
                   :map-desc (list :block start extent)))
    (defun submatrix (ima i0 j0 &optional n m)
      "Get a submatrix of a matrix \(2D array).  Start at I0 and J0 and extend
for N and M, respectively.  If N or M are omitted, run to the end of the array."
      (get-block ima (list i0 j0)
                 (list (or n (- (ima-dimension ima 0) i0))
                       (or m (- (ima-dimension ima 1) j0)))))
    (defun subvector (ima start &optional extent)
      "Get a subvector of a vector.  Start at START and extend to for EXTENT
elements."
      (get-block ima (list start) (list (or extent (- (ima-dimension ima 0)
                                                      start))))))

;;<<>>=
(def-generic-map
    (defmethod permute-indices (ima permutation)
      "Given an array, A, return an array, B, where the elements a_i...j =
b_n...m where indices n through m are the indices i through j permuted by
PERMUTATION."
      (map-indices ima
                   (/. (idx) (permute-list permutation idx))
                   (permute-list permutation (ima-dimensions ima))
                   :map-desc (list :perm permutation)))
    (defun transpose (ima)
      "Given a 2-D array, A, return an array, B, where the elements a_ij =
b_ji."
      (permute-indices ima '(1 0))))

;; @\section{Index maps that increase complexity}

;; <<>>=
(def-generic-map
    (defmethod add-index (ima n)
      (map-indices ima
                   (/. (idx) (list-remove-at n idx))
                   (list-insert-at n 1 (ima-dimensions ima)))))

;; Only works on the last index
;; <<>>=
(def-generic-map
    (defmethod raise-dimensionality (ima on-index &rest extent)
      (unless (eql on-index (- (length (ima-dimensions ima)) 1))
        (error "This only works on the last index of the array, for now."))
      (let* ((subspace-dims (cons (/ (nth on-index (ima-dimensions ima))
                                     (apply #'* extent))
                                  extent))
             (new-dims (append (subseq (ima-dimensions ima) 0 on-index)
                               subspace-dims)))
        (map-indices
         ima
         (/. (idx)
            (append
             (subseq idx 0 on-index)
             (list (linear-index (subseq idx on-index (1+ (length extent)))
                                 subspace-dims))))
         new-dims)))
    (defun group-elements-by (ima &rest extent)
      (apply #'raise-dimensionality ima 0 extent)))

;; @\subsection{Some other useful maps}

;; Now we define a few extremely useful index maps.  Here we define the maps
;; <<pbc-array>> and <<index-shift>>.  The map <<pbc-array>> wraps indices that
;; become too large back to zero and indices that become too small (negative)
;; back to the maximum value.  The map <<index-shift>> which applies a shift to
;; the indices.

;; I had to include a handful of utilities here.  This should be cleaned up
;; later (factored into a different file or library).

;;<<>>=
(defun sign (x)
  (cond ((< x 0) -1)
        (t 1)))

;;<<>>=
(defun outer-truncate (x &optional (divisor 1))
  "Find the nearest integer to \(/ X DIVISOR) that is not smaller in magnitude.
OUTER-TRUNCATE is to TRUNCATE as CEILING in to FLOOR, or something like that."
  (let ((rat (/ x divisor)))
    (* (sign rat) (ceiling (abs rat)))))

;;<<>>=
(def-generic-map
    (defmethod pbc-array (ima)
      (ima::map-indices ima (lambda (idx)
                              (iter
                                (for i in idx)
                                (for extent in (ima-dimensions ima))
                                (collecting
                                 (if (< i 0)
                                     (- i (* extent (outer-truncate i extent)))
                                     (- i (* extent (floor i extent)))))))
                        (ima-dimensions ima))))

;;<<>>=
(def-generic-map
    (defmethod index-shift (ima &rest shifts)
      (ima::map-indices ima (lambda (idx)
                              (iter
                                (for i in idx)
                                (for shift in shifts)
                                (collecting
                                 (- i shift))))
                        (ima-dimensions ima))))


;; @\subsection{Grouping IMAs}

;; @Inevitably, we find that we will want to combine two or more IMAs into one.
;; For instance, if we have a spanning set of vectors, we might want to form a
;; matrix out of them.  This can be achieved via <<group-imas>>.  This mapping
;; isn't really a mapping at all, in that it violates our design of accessing a
;; datastructure using a list of integers.  Instead it returns a new object of
;; type <<ima-group>> that hold a conglomeration of several IMAs.

;; <<>>=
(modf-def:defclass ima-group ()
  ((imas :accessor imas-of :initarg :imas)
   (index-placement :accessor index-placement-of :initarg :index-placement)))

(defmethod print-object ((array ima-group) stream)
  (print-ima array stream))

(defmethod ima-dimensions ((ima ima-group))
  (list-insert-at (index-placement-of ima)
                  (ima-dimension (imas-of ima) 0)
                  (ima-dimensions (imref (imas-of ima) 0))))

(defmethod imref ((ima ima-group) &rest idx)
  (apply #'imref (imref (imas-of ima) (nth (index-placement-of ima) idx))
         (list-remove-at (index-placement-of ima) idx)))
(defmethod (setf imref) (new-val (ima ima-group) &rest idx)
  (setf (apply #'imref (imref (imas-of ima) (nth (index-placement-of ima) idx))
               (list-remove-at (index-placement-of ima) idx))
        new-val))
;; (define-modf-method imref 1 (new-val (ima ima-group) &rest idx)
;;   (setf (apply #'imref (imref (imas-of ima) (nth (index-placement-of ima) idx))
;;                (list-remove-at (index-placement-of ima) idx))
;;         new-val))

;; <<>>=
(def-generic-map
    (defmethod group-imas (imas on-index)
      (make-instance 'ima-group
                     :imas imas
                     :index-placement on-index)))

;; <<>>=
(modf-def:defclass ima-append ()
  ((imas :accessor imas-of :initarg :imas)
   (index-placement :accessor index-placement-of :initarg :index-placement)))

(defmethod print-object ((array ima-append) stream)
  (print-ima array stream))

(defun replace-nth (nth list new-val)
  (if (> nth 0)
      (cons (car list) (replace-nth (- nth 1) (cdr list) new-val))
      (cons new-val (cdr list))))

(defmethod ima-dimensions ((ima ima-append))
  (replace-nth (index-placement-of ima)
               (ima-dimensions (imref (imas-of ima) 0))
               ;; We need a special case for list imas as they nest
               ;; transparently, meaning we can't use IN-IMA.
               (if (consp (imas-of ima))
                   (iter (for arr in (imas-of ima))
                     (summing (ima-dimension
                               arr (index-placement-of ima))))
                   (iter (for arr in-ima (imas-of ima))
                     (summing (ima-dimension
                               arr (index-placement-of ima)))))))

(defmethod imref ((ima ima-append) &rest idx)
  (let* ((app-index (nth (index-placement-of ima) idx))
         (arr (iter (for arr in (imas-of ima))
                (finding arr such-that
                         (< app-index (ima-dimension arr (index-placement-of ima))))
                (decf app-index (ima-dimension arr (index-placement-of ima))))))
    (apply #'imref arr (replace-nth (index-placement-of ima) idx app-index))))
(defmethod (setf imref) (new-val (ima ima-group) &rest idx)
  (let* ((app-index (nth (index-placement-of ima) idx))
         (arr (iter (for arr in (imas-of ima))
                (finding arr such-that
                         (< app-index (ima-dimension arr (index-placement-of ima))))
                (decf app-index (ima-dimension arr (index-placement-of ima))))))
    (setf (apply #'imref arr (replace-nth (index-placement-of ima) idx app-index))
          new-val)))
;; (define-modf-method imref 1 (new-val (ima ima-group) &rest idx)
;;   (setf (apply #'imref (imref (imas-of ima) (nth (index-placement-of ima) idx))
;;                (list-remove-at (index-placement-of ima) idx))
;;         new-val))

(def-generic-map
    (defmethod append-imas (imas on-index)
      (make-instance 'ima-append
                     :index-placement on-index
                     :imas imas)))

;; @\section{Unmapping and converting}

;; While <<imref>> is a very versatile method, it is common for users to want
;; send their data to code that isn't written with <<imref>>.  In order to
;; facilitate this, we have the idea of {\em unmapping}, or getting an instance
;; of data that is ``equivalent'', but without any emulated mappings.

(defmacro def-unmapper (type (ima-sym) &body body)
  "Define a set unmapping routines: one that unmaps given the name of the type,
one that unmaps given an example of the type, and one that notices the identity
unmap \(e.g. we want a list and we already have a list)."
  `(progn
     (defmethod unmap-into ((type (eql ',type)) ,ima-sym)
       ,@body)
     (defmethod unmap-into ((type ,type) ,ima-sym)
       (unmap-into ',type ,ima-sym))
     (defmethod unmap-into ((type (eql ',type)) (,ima-sym ,type))
       ,ima-sym)))

(defun unmap (ima)
  "Unmap an IMA into it's base type.  This searches down the layers of IMAs
until it finds a non-index-mapped-array structure, then unmaps into that."
  (unmap-into (iter (initially (setf arr ima))
                    (while (typep arr 'index-mapped-array))
                    (for arr = (data-of arr))
                    (finally (return arr)))
              ima))

;; @\section{Basic Utilities}

;; @It is useful to provide an easy copy mechanism for data structures, but as
;; many have stated, it is an ill defined problem in Lisp (and any language
;; except fully functional ones where you cannot copy).  I.e. If I copy, do I
;; make copies of the elements (deep copy), or leave them as references?  If you
;; are using lists as IMA, it is even more complicated; since nested lists are
;; IMAs, at what point does a copy procedure stop recursing?  We offer a
;; facility, <<copy-ima>> that leaves elements uncopied, and recurses lists
;; fully (not really happy about this one, but it is the only thing that makes
;; sense).

;; Even simpler, it is nice to provide a way of producing other IMAs that use
;; the same underlying data structure.  This is important since a user that has
;; picked a particular underlying data format (or one that has gone with some
;; default) doesn't want to incurr penalties of converting between formats.  For
;; this, we offer <<make-ima-like>>.


;;<<>>=
(defmethod make-ima-like ((ima index-mapped-array) &key dims)
  "Make an IMA with the same base type as the one given."
  (make-ima-like (iter (initially (setf arr ima))
                   (while (typep arr 'index-mapped-array))
                   (for arr = (data-of arr))
                   (finally (return arr)))
                 :dims dims))

;; @This is still irksome.  I would like to have the new array more like the
;; original, including element type for those data structures that support it.
;; I can't figure out a nice way to do this: allow low level work like element
;; types, but allow a fall back high level interface.

;; <<>>=
(defmethod make-ima-like (ima &key &allow-other-keys)
  (error "I don't know how to make an IMA like this one"))

;; <<>>=
(defmethod copy-ima (ima)
  "Copy any IMA."
  (let ((new (make-ima-like ima)))
    (setf (contents-of new) ima)
    new))

;; @\section{Backends}

;; @Index-Mapped-Arrays defines some basic support for Lisp types where the way
;; forward is clear.

;; @@ arrays.lisp +1

;; @@ lists.lisp +1

;; @\section{Mapping}

;; <<>>=
(defun map-ima (fn ima &rest more-imas)
  "Like MAPCAR, but for IMAs of arbitrary dimensionality.  The IMAs need to
match in dimensionality."
  (let ((ret-arr (make-ima-like ima)))
    (iter (for el in-ima ima with-index i)
      (setf (ima-flat-ref ret-arr i)
            (apply fn (cons el (mapcar (lambda (x) (ima-flat-ref x i)) more-imas)))))
    ret-arr))

;; @@ printer.lisp
