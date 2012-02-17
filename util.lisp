
(in-package :ima)

;; Some stuff stripped from my toolbox
(defun fsubvec (vec &optional (start 0) (end (length vec)))
  "Access a subsequence from a vector.  Do this with displaced arrays,
thus we are not consing or copying, but are pointing to the same
memory.  When we are dealing with functional code, this doesn't matter
and removing the copying is faster."
  (make-array (- end start)
              :displaced-to vec
              :displaced-index-offset start
              :element-type (array-element-type vec)
              :fill-pointer (and (array-has-fill-pointer-p vec)
                                 (fill-pointer vec))
              :adjustable (adjustable-array-p vec)))

(defmacro /. (args &rest body)
  "A little lambda replacement, the ``/.'' is stolen from the Qi
programming language.  Originally just to save typing and horizontal
space.  Extened it to allow for ignored arguments which are designated
by the ``_'' symbol."
  (let ((arglist (mapcar (lambda (arg) (if (and (symbolp arg)
                                           (equalp (symbol-name arg) "_"))
                                      (cons :gensym (gensym "IGNORED"))
                                      arg))
                         args)))
    `(lambda ,(mapcar (lambda (arg)
                   (if (and (consp arg)
                            (eql (car arg) :gensym))
                       (cdr arg)
                       arg)) arglist)
       (declare (ignore
                 ,@(mapcar #'cdr (remove-if-not
                                  (lambda (arg)
                                    (and (consp arg)
                                         (eql (car arg) :gensym)))
                                  arglist))))
       ,@body)))

(defun nd-index (linear extents)
  "Given a row major linear index and a list of array extents
\(dimensions) return a list of N-D array indicies."
  (iter (for ext on (append (cdr extents) (list 1)))
        (let* ((slab-size (apply #'* ext))
               (idx (floor linear slab-size)))
          (decf linear (* slab-size idx))
          (collect idx))))

(defun linear-index (index extents)
  (apply #'+
         (mapcar #'* index
                 (iter (for spacing on extents)
                       (collect (apply #'* (cdr spacing)))))))

(defun n-times (n func arg)
  "Self compose FUNC N times with and call on argument ARG."
  (declare (type (integer 0) n))
  (cond ((= n 0) arg)
        (t (funcall func (n-times (1- n) func arg)))))

(defun list-insert-at (position value list)
  "Insert VALUE at POSITION in LIST."
  (cond ((= position 0) (cons value list))
        ((null list) (error "POSITION is bigger than the length of the list"))
        (t (cons (car list)
                 (list-insert-at (1- position) value (cdr list))))))

(defun list-remove-at (position list)
  "Remove the value at POSITION in LIST."
  (cond ((= position 0) (cdr list))
        ((null list) (error "POSITION is bigger than the length of the list"))
        (t (cons (car list)
                 (list-remove-at (1- position) (cdr list))))))

(defun permute-list (perm list)
  "Permute LIST, sending the ith element of list to the the position
given by the ith element of PERM."
  (mapcar #'cdr
          (sort (mapcar #'cons perm list)
                #'< :key #'car)))

(defun arg-fiddle-list (perm &rest args)
  "Permute the ARGumentS according to the PERMutation; return as a
list."
  (permute-list perm args))

(defun arg-fiddle-mv (perm &rest args)
  "Permute the ARGumentS according to the PERMutation; return as
multiple values.  This is very useful for multiple-value-compose."
  (apply #'values (permute-list perm args)))

(defun replace-nth (nth list new-val)
  (if (> nth 0)
      (cons (car list) (replace-nth (- nth 1) (cdr list) new-val))
      (cons new-val (cdr list))))


