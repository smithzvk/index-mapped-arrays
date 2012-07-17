
(in-package :ima)

;; @\section{Printing Index Mapped Arrays}

;; @Okay, some explanation.  Printed representations of IMAs look like arrays,
;; duh.  If you have <<*print-readably*>> set, we just convert to a Lisp array
;; and print that.  Remember that <<*print-readably*>> is supposed to print to
;; something that, when read, will produce something similar to what was
;; printed.  This is about as close as we can get with IMA.

;;<<>>=
(defun print-ima (array stream)
  "Print the index mapped ARRAY to STREAM using the pretty printer."
  ;; Apapted from the SBCL printer
  (cond (*print-readably*
         (print (unmap-into 'array array) stream))
        (t (funcall (formatter "#~DD-IMA") stream (length (ima-dimensions array)))
         (labels ((output-guts (stream array)
                    (pprint-logical-block
                     (stream nil :prefix "(" :suffix ")")
                     (dotimes (i (ima-dimension array 0))
                       (when (not (= i 0))
                         (write-char #\Space stream)
                         (pprint-newline (if (ima-dimension array 0) :linear :fill) stream))
                       (if (= 1 (length (ima-dimensions array)))
                           (format stream "~S" (imref array i))
                           (output-guts stream (get-slice array 0 i)))))))
           (output-guts stream array))))
  array)

;;<<>>=
(defmethod print-object ((array index-mapped-array) stream)
  "Print the index mapped ARRAY to STREAM using the pretty printer."
  ;; Apapted from the SBCL printer
  (print-ima array stream))

;; @\section{Reading}

;; @Index mapped arrays provides an interface to existing data structures, and
;; when all else fails, provides a structure that wraps the data.  If you want
;; to read it in, all has not failed, just read it in with the data structures
;; native method of reading in objects.  This is why print readably just
;; converts to an array.
