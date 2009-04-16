;;; all the performance benchmarks
;;;
;;; Time-stamp: <2009-04-16 15:39:24 asf>


(in-package :cl-bench)


(defbench compiler
    :function 'cl-bench.misc:run-compiler
    :long "Compilation of the Gabriel benchmarks"
    :runs 10
    :disabled-for '(armedbear))

(defbench load-fasl
    :function 'cl-bench.misc:run-fasload
    :runs 272
    :disabled-for '(armedbear))

(defbench sum-permutations
    :long "traversal of a large, linked, self-sharing structure"
    :function 'cl-bench.misc:run-permutations
    :runs 7
    :disabled-for '(lispworks-personal-edition))

(defbench walk-list/seq
    :long "Walk a list of 2M fixnums that were sequentially allocated"
    :setup 'cl-bench.misc::setup-walk-list/seq
    :teardown 'cl-bench.misc::teardown-walk-list/seq
    :function 'cl-bench.misc:walk-list/seq
    :runs 300
    :disabled-for '(lispworks-personal-edition))

(defbench walk-list/mess
    :long "Walk a list of 2M fixnums that were mergesorted to spread pointers"
    :setup 'cl-bench.misc::setup-walk-list/mess
    :teardown 'cl-bench.misc::teardown-walk-list/mess
    :function 'cl-bench.misc:walk-list/mess
    :runs 140
    :disabled-for '(lispworks-personal-edition poplog))

(defbench boyer
  :group :gabriel
  :function 'cl-bench.gabriel:boyer
  :long "CONS-intensive logic-programming code"
  :runs 4)

(defbench browse
  :group :gabriel
  :function 'cl-bench.gabriel:browse
  :runs 300)

(defbench dderiv
  :group :gabriel
  :function 'cl-bench.gabriel:dderiv-run
  :runs 1500)

(defbench deriv
  :group :gabriel
  :function 'cl-bench.gabriel:deriv-run
  :runs 1500)

(defbench destructive
  :group :gabriel
  :function 'cl-bench.gabriel:run-destructive
  :runs 2000)

(defbench div2-test-1
  :group :gabriel
  :function 'cl-bench.gabriel:run-div2-test1
  :runs 3500)

(defbench div2-test-2
  :group :gabriel
  :function 'cl-bench.gabriel:run-div2-test2
  :runs 1950)

(defbench fft
  :group :gabriel
  :function 'cl-bench.gabriel:run-fft
  :runs 3066)

(defbench frpoly/fixnum
  :group :gabriel
  :function 'cl-bench.gabriel:run-frpoly/fixnum
  :runs 1600)

(defbench frpoly/bignum
  :group :gabriel
  :function 'cl-bench.gabriel:run-frpoly/bignum
  :runs 420)

(defbench frpoly/float
  :group :gabriel
  :function 'cl-bench.gabriel:run-frpoly/float
  :runs 1280)

(defbench puzzle
  :group :gabriel
  :long "Forest Baskett's Puzzle, exercising simple-vectors"
  :function 'cl-bench.gabriel:run-puzzle
  :runs 23793)

(defbench tak
  :group :gabriel
  :function 'cl-bench.gabriel:run-tak
  :runs 8000)

(defbench ctak
    :group :gabriel
    :long "TAKeuchi function using the catch/throw facility"
    :function 'cl-bench.gabriel:run-ctak
    :runs 17838)

(defbench trtak
    :group :gabriel
    :long "TAKeuchi function without tail recursion"
    :function 'cl-bench.gabriel:run-trtak
    :runs 8000)

(defbench takl
    :group :gabriel
    :long "TAKeuchi function with lists as counters"
    :function 'cl-bench.gabriel:run-takl
    :runs 1757)

(defbench stak
    :group :gabriel
    :long "TAKeuchi function with special variables instead of parameter passing"
    :function 'cl-bench.gabriel:run-stak
    :runs 1576)

(defbench fprint/ugly
    :group :gabriel
    :long "Pretty-printer and write operations to file, no *PRINT-PRETTY*"
    :function 'cl-bench.gabriel:fprint/ugly
    :runs 1300)

(defbench fprint/pretty
    :group :gabriel
    :long "Pretty-printer and write operations to file, with *PRINT-PRETTY*"
    :function 'cl-bench.gabriel:fprint/pretty
    :runs 180)

(defbench traverse
  :group :gabriel
  :long "Creates and traverses a tree structure"
  :function 'cl-bench.gabriel:run-traverse
  :runs 100)

(defbench triangle
  :group :gabriel
  :long "Puzzle solving (board game) using combinatorial search"
  :function 'cl-bench.gabriel:run-triangle
  :runs 35)

;; end of Gabriel benchmarks

(defbench richards
    :long "Operating system simulation"
    :function 'cl-bench.richards:richards
    :runs 40)

(defbench factorial
    :function 'cl-bench.math:run-factorial
    :runs 30000)

(defbench fib
    :function 'cl-bench.math:run-fib
    :runs 885)

(defbench fib-ratio
    :group :math
    :function 'cl-bench.math:run-fib-ratio
    :runs 75000)

(defbench ackermann
    :long "Calculating Ackermann's number (heavy recursion)"
    :group :math
    :function 'cl-bench.math:run-ackermann
    :runs 2)

(defbench mandelbrot/complex
    :group :math
    :long "Mandelbrot Set computation using complex numbers"
    :function 'cl-bench.math:run-mandelbrot/complex
    :runs 1620)

(defbench mandelbrot/dfloat
    :group :math
    :long "Mandelbrot Set computation using double-floats"
    :function 'cl-bench.math:run-mandelbrot/dfloat
    :runs 2970)

(defbench mrg32k3a
    :group :math
    :long "multiple recursive random number generator of l'Ecuyer"
    :function 'cl-bench.math:run-mrg32k3a
    :runs 160)

(defbench crc40
    :long "Cyclic redundancy check calculation using 40-bit integers"
    :function 'cl-bench.crc:run-crc40
    :runs 1)

(defbench bignum/elem-100-1000
    :group :bignum
    :function 'cl-bench.bignum:run-elem-100-1000
    :runs 36)

(defbench bignum/elem-1000-100
    :group :bignum
    :function 'cl-bench.bignum:run-elem-1000-100
    :runs 20)

(defbench bignum/elem-10000-1
    :group :bignum
    :function 'cl-bench.bignum:run-elem-10000-1
    :runs 14)

(defbench bignum/pari-100-10
    :group :bignum
    :function 'cl-bench.bignum:run-pari-100-10
    :runs 223)

(defbench bignum/pari-200-5
    :group :bignum
    :function 'cl-bench.bignum:run-pari-200-5
    :runs 84)

;; this one takes ages to run
#+slow-tests
(defbench bignum/pari-1000-1
    :group :bignum
    :short "bignum/pari-1000-1"
    :function 'cl-bench.bignum:run-pari-1000-1
    :runs 1)

(defbench pi-decimal/small
    :function 'cl-bench.bignum:run-pi-decimal/small
    :runs 556)

(defbench pi-decimal/big
    :group :bignum
    :function 'cl-bench.bignum:run-pi-decimal/big
    :runs 15)

(defbench pi-atan
    :group :bignum
    :function 'cl-bench.bignum:run-pi-atan
    :runs 1046)

(defbench pi-ratios
    :function 'cl-bench.ratios:run-pi-ratios
    :runs 6)

(defbench slurp-lines
    :long "Line-by-line read of a large file (mostly testing allocation speed)"
    :function 'cl-bench.hash:run-slurp-lines
    :runs 225)

(defbench hash-strings
    :function 'cl-bench.hash:hash-strings
    :runs 45)

(defbench hash-integers
    :function 'cl-bench.hash:hash-integers
    :runs 110)

(defbench boehm-gc
    :group :gc
    :function 'cl-bench.boehm-gc:gc-benchmark
    :runs 5
    :disabled-for '(lispworks-personal-edition))

(defbench deflate-file
    :function 'cl-bench.deflate:run-deflate-file
    :runs 1600)

;; these tests exceed the limited stack size in the trial version of LW
(defbench 1d-arrays
    :long "Adding together two vectors"
    :function 'cl-bench.arrays:bench-1d-arrays
    :setup 'cl-bench.arrays:bench-1d-arrays-setup
    :runs (* 100 10)
    :disabled-for '(lispworks-personal-edition))

(defbench 2d-arrays
    :long "Adding together two 2-dimensional arrays"
    :function 'cl-bench.arrays:bench-2d-arrays
    :setup 'cl-bench.arrays:bench-2d-arrays-setup
    :runs (* 4 10)
    :disabled-for '(lispworks-personal-edition))

(defbench 3d-arrays
    :long "Adding together two 3-dimensional arrays"
    :function 'cl-bench.arrays:bench-3d-arrays
    :setup 'cl-bench.arrays:bench-3d-arrays-setup
    :runs (* 1 10)
    :disabled-for '(lispworks-personal-edition))

;; Poplog seems to have a buggy implementation of bitvectors
(defbench bitvectors
    :long "BIT-XOR, BIT-AND on big bitvectors"
    :function 'cl-bench.arrays:bench-bitvectors
    :setup 'cl-bench.arrays:bench-bitvectors-setup
    :runs (* 11 700)
    :disabled-form '(lispworks-personal-edition poplog))

(defbench bench-strings
    :long "Allocate and fill large strings"
    :function 'cl-bench.arrays:bench-strings
    :setup 'cl-bench.arrays:bench-strings-setup
    :runs (* 2 50)
    :disabled-for '(lispworks-personal-edition))

(defbench fill-strings/adjust
    :short "fill-strings/adjustable"
    :long "Fill an adjustable array with characters"
    :setup 'cl-bench.arrays:bench-strings/adjustable
    :function 'cl-bench.arrays:bench-strings/adjustable
    :runs (* 2 10)
    :disabled-for '(lispworks-personal-edition))

;; as of 2002-01-20 this crashes CLISP, both release and CVS versions.
;; It exceeds maximum array size for both Allegro CL and LispWorks.
;; It takes AGES and consumes around 120MB RSS with Poplog
(defbench string-concat
    :long "WITH-OUTPUT-TO-STRING and much output"
    :function 'cl-bench.arrays:bench-string-concat
    :setup 'cl-bench.arrays:bench-string-concat-setup
    :runs (* 5 20)
    :disabled-for '(clisp allegro lispworks-personal-edition poplog))

(defbench search-sequence
    :long "FIND, FIND-IF, POSITION on a simple-vector"
    :function 'cl-bench.arrays:bench-search-sequence
    :setup 'cl-bench.arrays:bench-search-sequence-setup
    :runs (* 4 10)
    :disabled-for '(lispworks-personal-edition))

(defbench clos-defclass
    :short "CLOS/defclass"
    :long "Defines a class hierarchy"
    :function 'cl-bench.clos:run-defclass
    :runs 1)

(defbench clos-defmethod
    :short "CLOS/defmethod"
    :long "Defines methods on the class hierarchy"
    :function 'cl-bench.clos:run-defmethod
    :runs 1)

(defbench clos-instantiate
     :short "CLOS/instantiate"
     :long "Instantiates a complicated class hierarchy"
     :function 'cl-bench.clos:make-instances
     :runs 1)

(defbench clos-instantiate
     :short "CLOS/simple-instantiate"
     :long "Instantiates a simple class hierarchy"
     :function 'cl-bench.clos:make-instances/simple
     :runs 8000)

(defbench methodcalls
    :short "CLOS/methodcalls"
    :long "Make method calls against the created instances."
    :function 'cl-bench.clos:methodcalls/simple
    :runs 60)

(defbench methodcalls+after
    :short "CLOS/method+after"
    :long "Define after methods on our instances, then run some method calls"
    :function 'cl-bench.clos:methodcalls/simple+after
    :setup 'cl-bench.clos::setup-methodcalls/simple+after
    :runs 35)

(defbench methodcalls/complex
    :short "CLOS/complex-methods"
    :long "Run methodcalls with and method combination."
    :function 'cl-bench.clos:methodcalls/complex
    :runs 60
    :disabled-for '(clisp poplog))

(defbench eql-specialized-fib
    :long "Fibonnaci function implemented with EQL specialization"
    :function 'cl-bench.clos:run-eql-fib
    :runs 50)

;; this is really a test of the speed of loading a source file full of data
#+nil
(defbench ppcre-load/source
    :long "CL-PPCRE, Perl-compatible regular expressions: loading data file"
    :function '(load (make-pathname :directory '(:relative "files" "cl-ppcre")
                  :name "testdata"
                  :type "lisp"))
    :runs 1
    :disabled-for '(armedbear))

#+nil
(defbench ppcre-match
    :long "CL-PPCRE, perl-compatible regular expressions: matching speed"
    :function '(cl-ppcre-test:test)
    :runs 20
    :disabled-for '(armedbear))


(defbench make-list-sequential/push-nreverse
    :long "Cons up a sequential list with push/nreverse"
    :function `cl-bench.misc:make-list-sequential/push-nreverse
    :runs 5)

(defbench make-list-sequential/rplacd
    :long "Cons up a sequential list with push/nreverse"
    :function `cl-bench.misc:make-list-sequential/rplacd
    :runs 5)

#+(or sbcl cmu)
(defbench-untimed core-file-size
    :long "Core file size of the implementation (in bytes)"
    :function 'cl-bench.misc:core-file-size
    :runs 1)

;; EOF
