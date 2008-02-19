(in-package :autobench)

(defclass clisp (implementation git-vc-mixin architecture-mixin)
     ((name :allocation :class :initform "CLISP")
      (host-compiler :initform "gcc-3.3" :reader host-compiler)))

(defmethod version-from-directory :around ((impl clisp) dir)
  (let ((version (call-next-method)))
    (subseq version (mismatch version "clisp."))))

(defmethod clean-directory :around ((impl clisp) directory)
  (multiple-value-prog1 
   (call-next-method)
    (with-current-directory directory
      (invoke-logged-program (format nil "git-clean-~A" (impl-name impl))
                             *git-binary* `("clean" "-d" "-x" ".")))))

(defun pathname-with-host (pn impl)
  (merge-pathnames pn (make-pathname
                       :directory `(:relative ,(host-compiler impl)))))

(defmethod implementation-required-files ((impl clisp))
  '(#p"base/lisp.run"
    #p"base/lispinit.mem"))

(defmethod implementation-file-in-builddir ((impl clisp) pathname)
  (cdr
   (assoc pathname
          `((#p"base/lisp.run"
               . ,(pathname-with-host #p"base/lisp.run" impl))
            (#p"base/lispinit.mem"
               . ,(pathname-with-host #p"base/lispinit.mem" impl)))
          :test #'equal)))

(defmethod build-in-directory/arch ((impl clisp) dir (arch (eql :emulated-x86)))
  (with-current-directory dir
    (handler-case
        (invoke-logged-program
         "build-clisp"
         (merge-pathnames (merge-pathnames #p"scripts/run-in-32bit" *base-dir*)
                          *base-dir*)
         `(,(namestring (merge-pathnames #p"scripts/build-clisp"
                                         *base-dir*))
            (host-compiler impl)))
      (program-exited-abnormally ()
        (error 'implementation-unbuildable :implementation impl))))
  impl)

(defmethod build-in-directory/arch ((impl clisp) dir arch)
  (with-current-directory dir
    (handler-case
        (invoke-logged-program "build-clisp"
                               (merge-pathnames #p"scripts/build-clisp"
                                                *base-dir*)
                               `(,(host-compiler impl)))
      (program-exited-abnormally ()
        (error 'implementation-unbuildable :implementation impl))))
  impl)

(defmethod build-in-directory/arch ((impl clisp) dir (arch (eql :x86_64)))
  (with-current-directory dir
    (handler-case
        (invoke-logged-program "build-clisp"
                               (merge-pathnames #p"scripts/build-clisp"
                                                *base-dir*)
                               ;; XXX: won't build on AMD64 otherwise:
                               `(,(host-compiler impl) "--disable-mmap"))
      (program-exited-abnormally ()
        (error 'implementation-unbuildable :implementation impl))))
  impl)

(defun clisp-major-version (version)
    (parse-integer version
                   :end (position #\. version)))

(defun clisp-minor-version (version)
    (parse-integer version
                   :start (1+ (position #\. version))
                   :end (position #\- version)))

(defun prepare-bench-clisp-cmdline (impl shell-quote-p)
  `(,(shellquote
      (namestring
       (implementation-cached-file-name impl #p"base/lisp.run"))
      shell-quote-p)
     "-M" ,(shellquote
            (namestring
             (implementation-cached-file-name impl #p"base/lispinit.mem"))
            shell-quote-p)
     "-q" "-norc"
     ,(if (and (<= (clisp-major-version (impl-version impl)) 2)
               (<= (clisp-minor-version (impl-version impl)) 27))
          "-a"
          "-ansi")
     "-m" "200MB"
     ,(if (and (<= (clisp-major-version (impl-version impl)) 2)
               (<= (clisp-minor-version (impl-version impl)) 30))
          "-Efile"
          "-E")
     "iso-8859-1"
     "--"
     "--boink-implementation-type" ,(shellquote
                                     (implementation-translated-mode impl)
                                     shell-quote-p)
     "--boink-core-file" ,(shellquote
                           (namestring
                            (implementation-cached-file-name impl #p"base/lisp.run"))
                           shell-quote-p)
     ;; the next two are specific to clisp:
     "--boink-machine-instance" ,(shellquote (machine-instance) shell-quote-p)
     "--boink-implementation-version" ,(shellquote (impl-version impl)
                                                   shell-quote-p)))

(defmethod run-benchmark/arch ((impl clisp) (arch (eql :emulated-x86)))
  (with-unzipped-implementation-files impl
    (invoke-logged-program "bench-clisp"
                           (merge-pathnames #p"scripts/run-in-32bit" *base-dir*)
                           `("./run-clisp.sh"
                             ,@(prepare-bench-clisp-cmdline impl t)))))

(defmethod run-benchmark/arch ((impl clisp) arch)
  (with-unzipped-implementation-files impl
    (invoke-logged-program "bench-clisp"
                           "/usr/bin/env"
                           `("./run-clisp.sh"
                             ,@(prepare-bench-clisp-cmdline impl nil)))))

