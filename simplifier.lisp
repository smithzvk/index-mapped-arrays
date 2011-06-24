
(in-package :ima)

(defun in-order (list)
  (cond ((null (cdr list)) t)
        ((< (car list) (cadr list)) (in-order (cdr list)))
        (t nil) ))

(defun simplify (new-map ima)
  "A basic, hard-coded, pattern based simplifier."
  (cond ((and (not (eql :unknown new-map))
              (typep ima 'index-mapped-array)
              (not (eql :unknown (first (map-desc-of ima)))) )
         (let ((current-maps (map-desc-of ima)))
           (match (list new-map current-maps)
             ((list (list :perm p1) (list (list :perm p2) x))
              (let ((new-permutation (permute-list p1 p2)))
                (if (in-order new-permutation)
                    ;; We have effectively removed the other permutation
                    (data-of ima)
                    ;; A permutation of a permutation is the same as a
                    ;; permutation of the original list, permuted by the
                    ;; permuation of the permutation.  It's so simple, man
                    (permute-indices (data-of ima) new-permutation) )))
             ((list (list :block start1 extent1) (list (list :block start2 extent2) x))
              (get-block (data-of ima) (mapcar #'+ start1 start2)
                         extent1 ))
             (otherwise nil) )))
        (t nil) ))

