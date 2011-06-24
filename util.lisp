
(in-package :index-mapped-arrays)

;;; Utilities

(defun list-insert-at (position value list)
  "Insert VALUE at POSITION in LIST."
  (cond ((= position 0) (cons value list))
        ((null list) (error "POSITION is bigger than the length of the list"))
        (t (cons (car list)
                 (list-insert-at (1- position) value (cdr list)) ))))

(defun list-remove-at (position list)
  "Remove the value at POSITION in LIST."
  (cond ((= position 0) (cdr list))
        ((null list) (error "POSITION is bigger than the length of the list"))
        (t (cons (car list)
                 (list-remove-at (1- position) (cdr list)) ))))

(defun permute-list (perm list)
  "Permute LIST, sending the ith element of list to the the position
given by the ith element of PERM."
  (mapcar #'cdr
          (sort (mapcar #'cons perm list)
                #'< :key #'car )))

(defun arg-fiddle-list (perm &rest args)
  "Permute the ARGumentS according to the PERMutation; return as a
list."
  (permute-list perm args) )

(defun arg-fiddle-mv (perm &rest args)
  "Permute the ARGumentS according to the PERMutation; return as
multiple values.  This is very useful for multiple-value-compose."
  (apply #'values (permute-list perm args)) )

