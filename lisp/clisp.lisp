(in-package :autobench)

(defclass clisp (implementation git-vc-mixin architecture-mixin)
     ((:name :allocation :class :initform "CLISP")))

(defmethod implementation-required-files ((impl clisp))
  (declare (ignore impl))
  (list #p"clisp"
        #p"base/lisp.run"
        #p"base/lispmem.init"))

(defmethod implementation-file-in-builddir ((impl clisp) pathname)
  (declare (ignore impl))
  (cdr
   (assoc pathname
          ((#p"clisp" . #p"src/clisp")
           (#p"base/lisp.run" . #p"src/base/lisp.run")
           (#p"base/lispmem.init" . #p"src/base/lispmem.init")))))

;; TODO: version-from-directory
;; TODO: build-in-directory/arch
;; TODO: run-benchmark/arch