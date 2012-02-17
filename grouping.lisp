
(in-package :ima)

;; @\section{Grouping IMAs}

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

;; @The append unmapper unmaps according to the `left most' convention.  This
;; convention states that the ima will unmap according to how the left most ima
;; would unmap.

;;<<>>=
(defmethod base-type-of ((ima ima-group))
  (base-type-of (first (imas-of ima))))

;; <<>>=
(modf-def:defclass ima-append ()
  ((imas :accessor imas-of :initarg :imas)
   (index-placement :accessor index-placement-of :initarg :index-placement)))

(defmethod print-object ((array ima-append) stream)
  (print-ima array stream))

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

;; @The append unmapper unmaps according to the `left most' convention.
(defmethod base-type-of ((ima ima-append))
  (base-type-of (first (imas-of ima))))
