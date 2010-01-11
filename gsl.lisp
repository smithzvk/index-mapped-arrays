
;; gsl.lisp - GSL matrix vector interfaces for IMA
;; Copyright (C) 2008 Zach Smith

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.


(in-package :index-mapped-arrays)

(defclass* gsl-matrix ()
  (data
   dims ))

(defclass* gsl-vector ()
  (data
   dims ))

(defun out-of-bounds (dim idx)
  (cond ((null dim) nil)
        ((<= (car dim) (car idx)) t)
        (t (out-of-bounds (cdr dim) (cdr idx))) ))

(defmethod imref ((mat gsl-matrix) &rest idx)
  (if (out-of-bounds (dims-of mat) idx)
      (error "~a out of bounds of dimensions ~a" idx (dims-of mat))
      (apply #'gsl-bindings::matrix-get (data-of mat) idx) ))

(defmethod (setf imref) (val (mat gsl-matrix) &rest idx)
  (when (out-of-bounds (dims-of mat) idx)
    (error "~a out of bounds of dimensions ~a" idx (dims-of mat)) )
  (gsl-bindings::matrix-set (data-of mat) (car idx) (cadr idx) val)
  val )

(defmethod imref ((vec gsl-vector) &rest idx)
  (if (out-of-bounds (dims-of vec) idx)
      (error "~a out of bounds of dimensions ~a" idx (dims-of vec))
      (apply #'gsl-bindings::vector-get (data-of vec) idx) ))

(defmethod (setf imref) (val (vec gsl-vector) &rest idx)
  (when (out-of-bounds (dims-of vec) idx)
    (error "~a out of bounds of dimensions ~a" idx (dims-of vec)) )
  (gsl-bindings::vector-set (data-of vec) (car idx) val)
  val )

(defun make-gsl-vector (n &key
                        (element-type :double)
                        (initial-contents nil)
                        (initial-element 0) )
  "Create a GSL matrix and wrap it in an IMA object."
  (declare (ignore initial-element initial-contents element-type))
  (let* ((arr (gsl-bindings::vector-alloc n))
         (vec (make-instance 'gsl-vector :data arr :dims (list n))) )
    (tg:finalize vec (/. () (gsl-bindings::vector-free arr))) ))

(defun make-gsl-matrix (dims &key
                        (element-type :double)
                        (initial-contents nil)
                        (initial-element 0) )
  "Create a GSL matrix and wrap it in an IMA object."
  (declare (ignore initial-element initial-contents element-type))
  (let* ((arr (apply #'gsl-bindings::matrix-alloc dims))
         (mat (make-instance 'gsl-matrix :data arr :dims dims)) )
    (tg:finalize mat (/. () (gsl-bindings::matrix-free arr))) ))

(defmethod ima-dimensions ((gmat gsl-matrix))
  (dims-of gmat) )

(defmethod ima-dimension ((gmat gsl-matrix) n)
  (nth n (dims-of gmat)) )

(defmethod ima-dimensions ((gvec gsl-vector))
  (dims-of gvec) )

(defmethod ima-dimension ((gvec gsl-vector) n)
  (nth n (dims-of gvec)) )

(defmethod print-object ((mat gsl-matrix) str)
  (format str "#2D-IMG")
  (destructuring-bind (i-max j-max) (dims-of mat)
    (pprint-logical-block (str nil :prefix "(" :suffix ")")
      (dotimes (i i-max)
        (pprint-logical-block (str nil :prefix "(" :suffix ")")
          (dotimes (j j-max)
            (when (not (= j 0)) (write-char #\Space str))
            (format str "~A" (imref mat i j)) ))
        (when (not (= i (1- i-max)))
          (write-char #\Space str)
          (pprint-newline :linear str) )))))

(defmethod print-object ((vec gsl-vector) str)
  (format str "#IMG")
  (let ((max (ima-dimension vec 0)))
    (pprint-logical-block (str nil :prefix "(" :suffix ")")
      (dotimes (i max)
        (when (not (= i 0)) (write-char #\Space str))
        (pprint-newline :fill str)
        (format str "~A" (imref vec i)) ))))

