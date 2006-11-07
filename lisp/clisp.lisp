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

(defmethod build-in-directory/arch ((impl clisp) dir (arch (eql :emulated-x86)))
  (with-current-directory dir
    (invoke-logged-program
     "build-clisp"
     (merge-pathnames (merge-pathnames #p"scripts/run-in-32bit" *base-dir*)
                      `(,(namestring
                          (merge-pathnames #p"scripts/build-clisp"
                                           *base-dir*)))
                      *base-dir*)))
  impl)

(defmethod build-in-directory/arch ((impl clisp) dir arch)
  (with-current-directory dir
    (invoke-logged-program "build-clisp"
                           (merge-pathnames #p"scripts/build-clisp"
                                            *base-dir*)))
  impl)

(defun prepare-bench-clisp-cmdline (impl shell-quote-p)
  `(,(format nil "~A" (shellquote (implementation-cached-file-name impl "clisp")
                                  shell-quote-p))
     "-q" "-norc" "-ansi" "-m" "200MB" "-E" "latin1"
     "--"
     "--boink-implementation-type" (shellquote
                                    (implementation-translated-mode impl)
                                    shell-quote-p)
     "--boink-core-file" (shellquote
                          (namestring
                           (implementation-cached-file-name impl "lisp.run"))
                          shell-quote-p)))

(defmethod run-benchmark/arch ((impl sbcl) (arch (eql :emulated-x86)))
  (with-unzipped-implementation-files impl
    (invoke-logged-program "bench-clisp"
                           (merge-pathnames #p"scripts/run-in-32bit" *base-dir*)
                           `("./run-clisp.sh"
                             ,@(prepare-bench-clisp-cmdline impl t)))))

(defmethod run-benchmark/arch ((impl sbcl) arch)
  (with-unzipped-implementation-files impl
    (invoke-logged-program "bench-clisp"
                           "/usr/bin/env"
                           `("./run-clisp.sh"
                             ,@(prepare-bench-clisp-cmdline impl nil)))))

