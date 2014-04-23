
(in-package :ima)

;; @\section{Simplification of Mappings}

;;<<>>=
(defun in-order (list)
  (cond ((null (cdr list)) t)
        ((< (car list) (cadr list)) (in-order (cdr list)))
        (t nil)))

;; Removed cl-match as a dependency because the new Slime butts head with
;; cl-match's dependency, standard-cl, due to a nickname collision.  This is
;; stupid, but it is the way things are.

;;<<>>=
(defun simplify (new-map ima)
  "A basic, hard-coded, pattern based simplifier."
  (cond ((and (not (eql :unknown new-map))
              (typep ima 'index-mapped-array)
              (not (eql :unknown (first (map-desc-of ima)))))
         (let ((maps (list new-map (map-desc-of ima))))
           (cond
             ((and (eql :perm (caar maps))
                   (eql :perm (caaadr maps)))
              (let* ((p1 (second (car maps)))
                     (p2 (second (caadr maps)))
                     (new-permutation (permute-list p1 p2)))
                (if (in-order new-permutation)
                    ;; We have effectively removed the other permutation
                    (data-of ima)
                    ;; A permutation of a permutation is the same as a
                    ;; permutation of the original list, permuted by the
                    ;; permuation of the permutation.  It's so simple, man
                    (permute-indices (data-of ima) new-permutation))))
             ((and (eql :block (caar maps))
                   (eql :block (caaadr maps)))
              (let* ((start1 (second (car maps)))
                     (extent1 (third (car maps)))
                     (start2 (second (caadr maps))))
                (get-block (data-of ima)
                           (mapcar #'+ start1 start2)
                           extent1)))
             (t nil))))
        (t nil)))

