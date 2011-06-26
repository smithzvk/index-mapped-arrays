
(defpackage :ima-test
  (:use :stefil :cl :ima :iter)
  (:export #:run-tests) )

(in-package ima-test)

(in-root-suite)

(defsuite* ima-test)

(deftest run-tests ()
  (list-tests)
  (array-tests) )

(defun %compare-imas-by-element (ima1 ima2)
  (equal (ima-dimensions ima1) (ima-dimensions ima2))
  (iter (for el1 in-ima ima1)
        (for el2 in-ima ima2)
        (always (equal el1 el2)) ))

(defun compare-imas-by-element (&rest imas)
  (if (null (cdr imas))
      t
      (progn (and
              (%compare-imas-by-element (first imas) (second imas))
              (apply #'compare-imas-by-element (cdr imas)) ))))

(deftest mapping-tests (array)
  "Test simple mappings.  This tests the correctness of the mapping techniques."
  (is (compare-imas-by-element array array))
  (is (compare-imas-by-element array (transpose (transpose array))))
  (is (compare-imas-by-element (row-vector array 1) (column-vector (transpose array) 1)))
  (is (compare-imas-by-element (column-vector array 1) (row-vector (transpose array) 1)))
  (is (compare-imas-by-element (submatrix (submatrix array 0 0 2 2) 1 1 1 1)
                               (submatrix (submatrix array 1 1 2 2) 0 0 1 1) )))

(deftest mutation-test (2d-arr)
  "Test if mutations show up in all data structures that reference an array."
  ;; 2d stuff
  (let* ((row-vec (row-vector 2d-arr 1))
         (col-vec (column-vector 2d-arr 1))
         (sub-mat (submatrix 2d-arr 1 1 2 2))
         (sub-row-vec (get-block row-vec '(1) '(1)))
         (sub-col-vec (get-block col-vec '(1) '(1))) )
    ;; Setting an element with IMREF
    (setf (imref 2d-arr 1 1) 'test)
    (is (equal (imref 2d-arr 1 1) (imref row-vec 1)))
    (is (equal (imref 2d-arr 1 1) (imref col-vec 1)))
    (is (equal (imref 2d-arr 1 1) (imref sub-mat 0 0)))
    (is (equal (imref 2d-arr 1 1) (imref sub-row-vec 0)))
    (is (equal (imref 2d-arr 1 1) (imref sub-col-vec 0)))
    ;; Setting a row
    (setf (contents-of row-vec) '(this is test))
    (is (equal (imref 2d-arr 1 1) 'is))
    (is (equal (imref 2d-arr 1 1) (imref row-vec 1)))
    (is (equal (imref 2d-arr 1 1) (imref col-vec 1)))
    (is (equal (imref 2d-arr 1 1) (imref sub-mat 0 0)))
    (is (equal (imref 2d-arr 1 1) (imref sub-row-vec 0)))
    (is (equal (imref 2d-arr 1 1) (imref sub-col-vec 0)))
    ;; Setting a column
    (setf (column-vector 2d-arr 1) '(what about this))
    (is (equal (imref 2d-arr 1 1) 'about))
    (is (equal (imref 2d-arr 1 1) (imref row-vec 1)))
    (is (equal (imref 2d-arr 1 1) (imref col-vec 1)))
    (is (equal (imref 2d-arr 1 1) (imref sub-mat 0 0)))
    (is (equal (imref 2d-arr 1 1) (imref sub-row-vec 0)))
    (is (equal (imref 2d-arr 1 1) (imref sub-col-vec 0)))
    ;; Setting a block
    (setf (submatrix 2d-arr 0 0 2 2) '((uh oh) (what now)))
    (is (equal (imref 2d-arr 1 1) 'now))
    (is (equal (imref 2d-arr 1 1) (imref row-vec 1)))
    (is (equal (imref 2d-arr 1 1) (imref col-vec 1)))
    (is (equal (imref 2d-arr 1 1) (imref sub-mat 0 0)))
    (is (equal (imref 2d-arr 1 1) (imref sub-row-vec 0)))
    (is (equal (imref 2d-arr 1 1) (imref sub-col-vec 0))) ))


(defsuite* list-ima)

(defparameter *list-ima* '((1 2 3) (4 5 6) (7 8 9)))

(deftest list-smart-mapping (list-array)
  "This test is no ensure that intelligent choices are made for mapping if the
underlying data format supports it.  E.g. a list IMA's row vectors are just
returned rather than mapped via the more general mechanism."
  (is (typep list-array 'cons)
      "You passed some to list-smart-mapping that is not a list IMA" )
  (is (typep (get-block list-array '(1 0) '(2 3)) 'cons))
  (is (typep (row-vector list-array 1) 'cons))
  (is (typep (get-block (row-vector list-array 1) '(1) '(2)) 'cons)) )

(deftest list-tests ()
  (mapping-tests *list-ima*)
  (list-smart-mapping *list-ima*)
  (mutation-test *list-ima*) )


(defsuite* array-ima)

(defparameter *array-ima* #2A((1 2 3) (4 5 6) (7 8 9)))

(deftest array-smart-mapping (array-ima)
  "This test is no ensure that intelligent choices are made for mapping if the
underlying data format supports it.  E.g. a list IMA's row vectors are just
returned rather than mapped via the more general mechanism."
  (is (typep array-ima 'array)
      "You passed some to array-smart-mapping that is not an array IMA" )
  (is (typep (row-vector array-ima 1) 'array)) ) 

(deftest array-tests ()
  (mapping-tests *array-ima*)
  (array-smart-mapping *array-ima*)
  (mutation-test *array-ima*) )
