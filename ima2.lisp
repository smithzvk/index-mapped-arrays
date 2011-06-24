
;; @\documentclass{article}

;; @\begin{document}

;; @\title{Index Mapped Array (or any data really)}

;; @\maketitle

;; @\begin{abstract}

;; Index mapped arrays or IMA is a well integrated, general method of
;; dealing with arrays in Common Lisp.  It provides facilities to
;; perform common mappings on lisp data, and foreign data, and
;; provides a simple interface to extend this behavior.  It is based
;; around CLOS methods and extensions are written as methods
;; specializing on your data type.  IMA tries its best to be more than
;; a simple wrapper around a data type and will operate on the raw
;; type whenever possible, and falls back on an index mapping
;; mechanism as versatile as Common Lisp itself.

;; @\end{abstract}

(in-package :ima)

;; @\section{Introduction}

;; @\subsection{A word on the general structure}

;; As stated, IMA is built on CLOS.  I use it in some unorthodox ways,
;; however.  The basic idea is that any object whose data can be
;; mapped onto a sequence of numbers can be thought of as an IMA.
;; That means that arrays, lists, strings, hash tables, even
;; structures and classes, can be thought of as IMAs.  So in some
;; sense, the class <<index-mapped-array>> should be thought of as a
;; superclass to almost anything; somewhere up there by T.  Since I
;; didn't want to edit the class hierarchy (and doing so is not
;; portable, right?), many of the IMA methods, <<ima-dimensions>>,
;; <<get-vector>>, any map really, specialize on class T.  These are
;; keeping with the normal CLOS way, if you want to modify the
;; behavior define a more specific method and (possibly) hijack the
;; method chain.

;; There is another way that I am using CLOS.  I also wish to have
;; methods that search from least specific to most specific.  This
;; could be implemented as a method combination, but it is beyond my
;; ability.  So as it is, I specialize on class
;; <<index-mapped-array>>, and that calls the method of the underlying
;; data type.  Methods that behave this way are <<imref>>,
;; <<unmap-into>>, and <<make-ima>>.

;; @\subsection{Building your own IMA}

;; @One of the best features of IMA is that it is easy to define your
;; own IMA interface to some data that the implementors have never
;; even thought of.  You just have to define methods for the
;; interfaces you want to use.  A full IMA interface for any data
;; requires you to define <<ima-dimensions>>, <<ima-dimension>>,
;; <<imref>>, <<(setf imref)>> if you want to set elements, <<immod>>
;; if you want to provide a functional modification method, and a set
;; of <<unmap-into>> methods and <make-ima>> methods.  It sounds like
;; a lot, but it really isn't.  Some methods can be omitted with
;; decreased functionality.

;; @The functions <<ima-dimension>>, <<ima-dimensions>>, and <<imref>>
;; are necessary to hook your data structure into the generic
;; interface.  They are absolutely required.

;; @The functions <<(setf imref)>> and <<immod>> offer a way to mutate
;; or functionally change the IMA.  Typically one is used more often
;; than the other because a data structure either aims to be
;; functional or mutable.  If you are using a mutable data format, it
;; might be nice if you also include an <<immod>> definition, but if
;; you using an immutable data structure, it is often impossible to
;; provide a {\em setf} that behaves sanely.

;; @The <<unmap-into>> functions allow the user to recieve a version
;; of the array that is element by element equal (in some sense of the
;; word) to the IMA.  This might entail no work at all, i.e. you are
;; not performing any emulated index mappings, or it may entail a
;; complete copying of the array.  It also allows for transforming
;; between possible IMA representations, such as from an array to
;; nested lists or a GSL matrix.  Since this can return you the same
;; instance, it is helpful to have a <<make-ima>> method which
;; gaurantees that the memory will be freshly allocated (but the
;; contents not copied, for copying, see <<copy-ima>>).

;;; Index mapping

;; @\section{The IMA class and index mapping}

;;<<>>=
(defclass index-mapped-array ()
  ((backend :initarg :backend :accessor backend-of)
   (data :initarg :data :accessor data-of)
   (dims :initarg :dims :accessor dims-of)
   (map-desc :initarg :map-desc :accessor map-desc-of)
   (map :initarg :map :accessor map-of) ))

(locally
    (declare (optimize (speed 3)))
  (defmethod ima-dimension (ima axis)
    "Return the length of IMA along AXIS."
    (nth axis (ima-dimensions ima)) )
  (defmethod ima-dimensions (ima)
    "Return the extents of the IMA."
    (dims-of ima) )
  (defmethod imref ((ima index-mapped-array) &rest idx)
    "Get the element of IMA at indices IDX."
    (declare (optimize (speed 3) (debug 1) (compilation-speed 0) (safety 1) (space 0))
             (dynamic-extent idx) )
    (apply #'imref (data-of ima) (funcall (the function (map-of ima)) idx)) )
  (defmethod (setf imref) (val (ima index-mapped-array) &rest idx)
    "Set the value of IMA at indices IDX to value VAL."
    (setf (apply #'imref (data-of ima) (funcall (the function (map-of ima)) idx)) val) ))

;; @{\em Note:} If you are using your IMA in a functional way with <<immod>>,
;; you should probably not be using <<(setf imref)>>.  <<(setf imref)>> changes
;; the original data, <<immod>> returns a new structure and makes no promises on
;; whether structure is shared or not.  You could imagine that you might get
;; something back from <<immod>> that shares part of it's structure with the
;; underlying data.  Then when you set, it might be changing the original data,
;; or not.  When you look at that orignial array, it is very likely that it has
;; been corrupted.

;; <<immod>> is the analog of the function <<(setf imref)>>.  It returns a new
;; IMA with the given, single element changes to a new value.  Using Modf, we
;; can define methods that modify general map places, like <<column-vector>> for
;; instance.

(defmethod immod (val (ima index-mapped-array) &rest idx)
  "This is an index-mapped-array strcuture, so we will pass the work down to the
underlying structure."
  (map-indices (apply #'immod val (data-of ima) (funcall (map-of ima) idx))
               (map-of ima)
               (dims-of ima)
               :map-desc (map-desc-of ima) ))

(defmethod ima-flat-ref (ima index)
  "Allows you to access the data of an IMA in a linear fashion.  No guarantees
are made as to the order in which the elements are ordered \(this may change in
the future if it becomes benefitial)."
  (apply #'imref ima
         (nd-index index (ima-dimensions ima)) ))

(defmethod (setf ima-flat-ref) (val ima index)
  "Allows you to set the data of an IMA by referencing the data in a linear
fashion."
  (setf (apply #'imref ima
               (nd-index index (ima-dimensions ima)) )
        val ))

(defmethod flat-ima-size (ima)
  "Returns the linear size of an IMA."
  (apply #'* (ima-dimensions ima)) )

(defun map-indices (object map dims &key map-desc backend)
  "Create an object of type index-mapped-array.  This is basically a fall back
for times when you want a mapping that a data type can't do natively."
  (make-instance 'index-mapped-array
                 :data object
                 :map map
                 :dims dims
                 :backend (cond (backend backend)
                                ((arrayp object) 'array)
                                ((consp object) 'list) )
                 :map-desc map-desc ))

;; @\section{Common (built in) maps}

;; @This is an ugly, fragile macro that declares generic imref and
;; (setf imref) and any nicer wrapper functions.  {\bf NOTE:} Function
;; bodies must be explicit, and must contain nothing but a call to the
;; underlying mapping method.  Some variable capture possibilties
;; which need to be fixed.

(defmacro def-generic-map ((defmethod name (&rest args) &body body)
                           &rest convenience-functions )
  "Define a generic map which includes an IMREF method definition and a \(SETF
IMREF) method definition."
  (declare (ignore defmethod))
  (with-gensyms (mapped-data-sym mapped-i-sym setf-new-val-sym "DEF-GENERIC-MAP-")
    `(progn
       (defmethod ,name (,@args)
         ,@body )
       (defmethod (setf ,name) (,setf-new-val-sym ,@args)
         (let ((,mapped-data-sym ,(if (member '&rest args)
                                      (append (list 'apply `(function ,name))
                                              (remove '&rest args) )
                                      (cons name args) )))
           (iter (for dest-el in-ima ,mapped-data-sym with-index ,mapped-i-sym)
                 (for el in-ima ,setf-new-val-sym)
                 (setf (apply #'imref ,mapped-data-sym
                              (nd-index ,mapped-i-sym
                                        (ima-dimensions ,mapped-data-sym) ))
                       el ))
           ,setf-new-val-sym ))
       ,@(iter (for (defun conv-name conv-args . conv-body) in convenience-functions)
               (collecting
                 `(progn (defun ,conv-name ,conv-args ,@conv-body)
                         (defun (setf ,conv-name) ,(cons setf-new-val-sym conv-args)
                           (setf ,@conv-body ,setf-new-val-sym) )))))))

;;<<>>=
(def-generic-map
    (defmethod get-projection (ima n val)
      "This reduces the dimensionality, D, to D-1."
      (map-indices ima (/. (idx) (append (subseq idx 0 n) (list val) (subseq idx n)))
                   (let ((count -1))
                     (remove-if (/. (_) (= n (incf count))) (ima-dimensions ima)) ))))

;;<<>>=
(def-generic-map
    (defmethod get-vector (ima n &rest fixed)
      "This reduces the dimensionality to 1, i.e. a vector."
      (map-indices ima (/. (idx)
                          (append (subseq fixed 0 n) idx (subseq fixed n)) )
                   (list (ima-dimension ima n)) ))
    (defun column-vector (ima n) (get-vector ima 0 n))
    (defun row-vector (ima n) (get-vector ima 1 n)))

;;<<>>=
(def-generic-map
    (defmethod get-diagonal (ima)
      "Map to the vector representing the diagonal of a N-dimensional cubic
array."
      (map-indices ima (/. (idx)
                          (make-list
                           (length (ima-dimensions ima))
                           :initial-element (car idx) ))
                   (list (ima-dimension ima 0)) )))

;;<<>>=
(def-generic-map
    (defmethod get-cross-diagonal (ima)
      "For 2-D arrays, return the opposite diagonal of the matrix, which crosses
the matrix diagonal."
      (unless (= (length (ima-dimensions ima)) 2)
        (error "The cross diagonal is only unique for 2D arrays \(matrices)") )
      (map-indices ima (/. (idx)
                          (let ((n (first (ima-dimensions ima))))
                            (list (- n (car idx) 1) (car idx)) ))
                   (list (ima-dimension ima 0)) )))

;;<<>>=
(def-generic-map
    (defmethod get-block (ima start extent)
      "Get a sub-block of the array.  This does not change the dimensionality."
      (map-indices ima (/. (idx) (mapcar #'+ start idx)) extent) )
    (defun submatrix (ima i0 j0 n m) (get-block ima (list i0 j0) (list n m))) )

;;<<>>=
(def-generic-map
    (defmethod transpose (ima)
      "Given a 2-D array, A, return an array, B, where the elements a_ij =
b_ji."
      (map-indices ima (/. (idx) (reverse idx)) (reverse (ima-dimensions ima))) ))

;; @<<self-map>> is a trick to allow you to {\em setf} entire IMA
;; contents.  {\em contents-of} is a more plain english desciptive
;; name of the facility.

;;<<>>=
(def-generic-map
    (defmethod self-map (ima)
      "Return an identity map of the IMA.  Useful if you want to SETF an entire
array."
      ima )
    (defun contents-of (ima)
      "Return an identity map of the IMA.  Useful if you want to SETF an entire
array."
      (self-map ima) ))

;; @\section{Unmapping and converting}

;; While <<imref>> is a very versatile method, it is common for users
;; to want send their data to code that isn't written with <<imref>>.
;; In order to facilitate this, we have the idea of {\em unmapping},
;; or getting an instance of data that is ``equivalent'', but without
;; any emulated mappings.

(defmacro def-unmapper (type (ima-sym) &body body)
  "Define a set unmapping routines: one that unmaps given the name of the type,
one that unmaps given an example of the type, and one that notices the identity
unmap \(e.g. we want a list and we already have a list)."
  `(progn
     (defmethod unmap-into ((type (eql ',type)) ,ima-sym)
       ,@body )
     (defmethod unmap-into ((type ,type) ,ima-sym)
       (unmap-into ',type ,ima-sym) )
     (defmethod unmap-into ((type (eql ',type)) (,ima-sym ,type))
       ,ima-sym )))

(defun unmap (ima)
  "Unmap an IMA into it's base type.  This searches down the layers of IMAs
until it finds a non-index-mapped-array structure, then unmaps into that."
  (unmap-into (iter (initially (setf arr ima))
                    (while (typep arr 'index-mapped-array))
                    (for arr = (data-of arr))
                    (finally (return arr)) )
              ima ))

;; @\section{Basic Utilities}

;; @It is useful to provide an easy copy mechanism for data
;; structures, but as many have stated, it is an ill defined problem
;; in Lisp (and any language except fully functional ones where you
;; cannot copy).  I.e. If I copy, do I make copies of the elements
;; (deep copy), or leave them as references?  If you are using lists
;; as IMA, it is even more complicated; since nested lists are IMAs,
;; at what point does a copy procedure stop recursing?  We offer a
;; facility, <<copy-ima>> that leaves elements uncopied, and recurses
;; lists fully (not really happy about this one, but it is the only
;; thing that makes sense).

;; Even simpler, it is nice to provide a way of producing other IMAs
;; that use the same underlying data structure.  This is important
;; since a user that has picked a particular underlying data format
;; (or one that has gone with some default) doesn't want to incurr
;; penalties of converting between formats.  For this, we offer
;; <<make-ima>>.


;;<<>>=
(defmethod make-ima ((ima index-mapped-array) &key dims)
  "Make an IMA with the same base type as the one given."
  (make-ima (iter (initially (setf arr ima))
                  (while (typep arr 'index-mapped-array))
                  (for arr = (data-of arr))
                  (finally (return arr)) )
            dims ))

;; @This is still irksome.  I would like to have the new array more
;; like the original, including element type for those data structures
;; that support it.  I can't figure out a nice way to do this: allow
;; low level work like element types, but allow a fall back high level
;; interface.

(defmacro def-maker (type (dims &rest keys) &body body)
  "Define MAKE-IMA methods for TYPE."
  (with-gensyms (ima-or-type)
    `(progn
       (defmethod make-ima ((,ima-or-type ,type)
                            &key ,dims ,@keys &allow-other-keys)
         (let ((,dims (if ,dims ,dims (ima-dimensions ,ima-or-type))))
           ,@body ))
       (defmethod make-ima ((,ima-or-type (eql ',type))
                            &key ,dims ,@keys &allow-other-keys)
         (declare (ignore ,ima-or-type))
         ,@body ))))

;;<<>>=
;; (defmethod copy-ima (ima)
;;   "This really doesn't copy, it references the old array.  This is
;;   useful for functional only IMAs."
;;   (let ((new (make-ima ima)))
;;     (setf (contents-of new) ima) ))

(defmethod copy-ima (ima)
  "This is the fallback IMA copy method."
  (let ((new (make-ima ima)))
    (iter (for el1 in-ima ima)
          (for i from 0)
          (setf (ima-flat-ref new i) el1) )
    new ))

;; @\section{Backends}

;; @\subsection{Arrays}

(defmethod ima-dimension ((ima array) axis)
  (array-dimension ima axis) )
(defmethod ima-dimensions ((ima array))
  (array-dimensions ima) )
(defmethod imref ((ima array) &rest idx)
  (declare (optimize (speed 3) (debug 1) (compilation-speed 0) (safety 1) (space 0))
           (dynamic-extent idx)
           (ftype (function (array &rest cons) t) imref)
           (inline imref) )
  (apply #'aref ima idx) )
(defmethod (setf imref) (val (ima array) &rest idx)
  (setf (apply #'aref ima idx) val) )

(defmethod immod (val (ima array) &rest idx)
  (let ((new (tb::copy-array ima)))
    (setf (apply #'aref new idx) val)
    new ))

(def-unmapper array (ima)
  (let* ((arr (make-array (ima-dimensions ima))))
    (if (= 1 (length (ima-dimensions ima)))
        ;; vector, treat it specially
        (iter (for i below (ima-dimension ima 0))
              (setf (aref arr i) (imref ima i)) )
        ;; an array of 2 or more dimensions
        (iter (for i below (apply #'* (ima-dimensions ima)))
              (setf (row-major-aref arr i)
                    (apply #'imref ima (nd-index i (ima-dimensions ima))) )))
    arr ))

(def-maker array (dims (element-type t))
  (make-array dims :element-type element-type) )

(defmethod get-vector ((ima array) n &rest fixed)
  (let ((row-direction (1- (length (array-dimensions ima)))))
    (if (= n row-direction)
        (make-array (array-dimension ima row-direction)
                    :displaced-to ima
                    :displaced-index-offset (apply #'array-row-major-index
                                                   ima (append fixed (list 0)) ))
        (call-next-method) )))

(defmethod get-block ((ima vector) start extent)
  (fsubvec ima (first start) (+ (first start) (first extent))) )


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


;;; Printing/reading

(defmethod print-object ((array index-mapped-array) stream)
  "Print the index mapped ARRAY to STREAM using the pretty printer."
  ;; Apapted from the SBCL printer
  (cond (*print-readably*
         (print (unmap-into 'array array)) )
        (t (funcall (formatter "#~DD-IMA") stream (length (ima-dimensions array)))
         (labels ((output-guts (stream array)
                    (pprint-logical-block
                     (stream nil :prefix "(" :suffix ")")
                     (dotimes (i (ima-dimension array 0))
                       (when (not (= i 0))
                         (write-char #\Space stream)
                         (pprint-newline (if (ima-dimension array 0) :linear :fill) stream) )
                       (if (= 1 (length (ima-dimensions array)))
                           (format stream "~A" (imref array i))
                           (output-guts stream (get-projection array 0 i)) )))))
           (output-guts stream array) ))))

