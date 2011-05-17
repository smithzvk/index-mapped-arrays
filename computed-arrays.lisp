
(in-package :ima)

;; @\section{Computed arrays}

(defclass computed-array (index-mapped-array)
  ((mod :initarg :mod :accessor mod-of)
   (setter :initarg :setter :accessor setter-of) ))

(defmethod imref ((ima computed-array) &rest idx)
  (funcall (mod-of ima) ima idx) )
(defmethod (setf imref) (val (ima computed-array) &rest idx)
  (funcall (setter-of ima) val ima idx) )

(defun compute-array (object map dims mod setter &key map-desc backend)
  (make-instance 'computed-array
                 :data object
                 :map map
                 :mod mod
                 :setter setter
                 :dims dims
                 :backend (cond (backend backend)
                                ((arrayp object) 'array)
                                ((consp object) 'list) )
                 :map-desc map-desc ))

