;; benchmarks speed of array and sequence operations
;;
;; Author: Eric Marsden <emarsden@laas.fr>
;; Time-stamp: <2009-04-16 13:46:50 asf>
;;
;;
;; Timing tests for creation, initialization, access and garbage
;; collection for arrays, vectors, bitvectors and strings.
;;
;; NOTE: be careful running these in CMUCL on Linux with sizes larger
;; than your RAM; you will most likely crash your machine.


(in-package :cl-bench.arrays)

(defmacro def-array-benchmark-fun ((name (size-var size-default))
                                   (&rest bindings)
                                   &body body)
  (let ((size-arg (gensym)))
    `(let ((,size-var nil)
           ,@(mapcar #'first bindings))
       (defun ,(intern (format nil "~A-SETUP" name))
           (&optional (,size-arg ,size-default))
         (setf ,size-var ,size-arg
               ,@(mapcan #'identity bindings))
         (values))
       (defun ,name ()
         ,@body))))

(def-array-benchmark-fun (bench-1d-arrays (size 100000))
    ((ones (make-array size :element-type '(integer 0 1000) :initial-element 1))
     (twos (make-array size :element-type '(integer 0 1000) :initial-element 2))
     (threes (make-array size :element-type '(integer 0 2000))))
  (declare (fixnum size))
  (dotimes (pos size)
    (setf (aref threes pos) (+ (aref ones pos) (aref twos pos))))
  (assert (null (search (list 4 5 6) threes)))
  (values))

(def-array-benchmark-fun (bench-2d-arrays (size 2000))
    ((ones (make-array (list size size) :element-type '(integer 0 1000) :initial-element 1))
     (twos (make-array (list size size) :element-type '(integer 0 1000) :initial-element 2))
     (threes (make-array (list size size) :element-type '(integer 0 2000))))
  (declare (fixnum size))
  (dotimes (i size)
    (dotimes (j size)
      (setf (aref threes i j)
            (+ (aref ones i j) (aref twos i j)))))
  (assert (eql 3 (aref threes 3 3)))
  (values))

(def-array-benchmark-fun (bench-3d-arrays (size 200))
    ((ones (make-array (list size size size) :element-type '(integer 0 1000) :initial-element 1))
     (twos (make-array (list size size size) :element-type '(integer 0 1000) :initial-element 2))
     (threes (make-array (list size size size) :element-type '(integer 0 2000))))
  (declare (fixnum size))
  (dotimes (i size)
    (dotimes (j size)
      (dotimes (k size)
        (setf (aref threes i j k)
              (+ (aref ones i j k) (aref twos i j k))))))
  (assert (eql 3 (aref threes 3 3 3)))
  (values))

(def-array-benchmark-fun (bench-bitvectors (size 1000000))
    ((zeros (make-array size :element-type 'bit :initial-element 0))
     (ones  (make-array size :element-type 'bit :initial-element 1))
     (xors  (make-array size :element-type 'bit)))
  (declare (fixnum size))
  (bit-xor zeros ones xors)
  (bit-nand zeros ones xors)
  (bit-and zeros xors)
  (values))

(def-array-benchmark-fun (bench-strings (size 1000000))
    ((zzz (make-string size :initial-element #\z))
     (xxx (make-string size)))
  (declare (fixnum size))
  (and (fill xxx #\x)
       (replace xxx zzz)
       (search "xxxd" xxx)
       (nstring-upcase xxx))
  (values))

(def-array-benchmark-fun (bench-strings/adjustable (size 200000))
    ()
  (declare (fixnum size))
  (let ((sink (make-array 10 :element-type 'character :adjustable t :fill-pointer 0)))
    (dotimes (i size)
      (vector-push-extend (code-char (mod i 128)) sink)))
  (values))

;; certain implementations such as OpenMCL have an array (and thus
;; string) length limit of (expt 2 24), so don't try this on humungous
;; sizes
(def-array-benchmark-fun (bench-string-concat (size 200000))
    ()
  (declare (fixnum size))
  (let ((len (length
              (with-output-to-string (string)
                (dotimes (i size)
                  (write-sequence "hi there!" string))))))
    (assert (eql len (* size (length "hi there!")))))
  (values))

(def-array-benchmark-fun (bench-search-sequence (size 1000000))
    ((haystack (make-array size :element-type '(integer 0 1000))))
  (declare (fixnum size))
  (dotimes (i size)
    (setf (aref haystack i) (mod i 1000)))
  (assert (null (find -1 haystack :test #'=)))
  (assert (null (find-if #'minusp haystack)))
  (assert (null (position -1 haystack :test #'= :from-end t)))
  (loop :for i :from 20 :to 900 :by 20
        :do (assert (eql i (position i haystack :test #'=))))
  (assert (eql 0 (search #(0 1 2 3 4) haystack :end2 1000 :from-end t)))
  (values))


;; EOF
