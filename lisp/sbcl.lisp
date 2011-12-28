(in-package :autobench)

(defclass sbcl (implementation git-vc-mixin architecture-mixin)
     ((name :allocation :class :initform "SBCL")))

;; overrides the git-vc-mixin version with something more sensible
(defmethod version-from-directory ((impl sbcl) directory)
  (declare (ignore impl))
  (let ((*read-eval* nil))
    (unless (probe-file #p"version.lisp-expr")
      (with-current-directory directory
        (invoke-logged-program "generate-version" "/bin/bash"
                               `("-c" ". generate-version.sh ; generate_version")
                               :environment (append (sb-ext:posix-environ)
                                                    (list "NO_GIT_HASH_IN_VERSION=true")))))
    (with-open-file (f #p"version.lisp-expr" :direction :input)
      ;; some revisions had a stray ' in version.lisp-expr.  ugh!
      (let ((version (read f)))
        (cond ((and (listp version) (eql 'quote (first version)))
               (second version))
              (t version))))))

(defun invoke-with-customize-target-features (features function)
  (unwind-protect
      (if features
          (progn
            (with-open-file (f #P"customize-target-features.lisp" :direction :output :if-exists :supersede)
              (let ((*package* (find-package :autobench)))
                (print `(lambda (features)
                          (flet ((enable (x)
                                   (pushnew x features))
                                 (disable (x)
                                   (setf features (remove x features))))
                            ,@(loop for feature in features
                                    for disabled-feature = (when (and (consp feature)
                                                                      (eql (first feature) 'not))
                                                             (second feature))
                                    if (not (null disabled-feature))
                                      collect `(disable ,disabled-feature)
                                    else
                                      collect `(enable ,feature))
                            features))
                       f)))
            (funcall function))
          (funcall function))
    (when (probe-file #P"customize-target-features.lisp")
      (delete-file #P"customize-target-features.lisp"))))

(defmacro with-customize-target-features (features &body body)
  `(invoke-with-customize-target-features ,features
                                          (lambda ()
                                            ,@body)))

(defmethod clean-directory :around ((impl sbcl) dir)
  (declare (ignore impl))
  (with-current-directory dir
    (when (probe-file #p"tools-for-build/grovel-headers")
      (delete-file #p"tools-for-build/grovel-headers"))))

(defmacro with-sbcl-build-setup ((implementation directory) &body body)
  (let ((arch (gensym))
        (features (gensym)))
   `(with-current-directory ,directory
      (destructuring-bind (&key ((:arch ,arch)) ((:features ,features))) (impl-mode ,implementation)
        (declare (ignorable ,arch))
        (with-customize-target-features ,features
          (handler-case (progn ,@body)
            (program-exited-abnormally (c)
              (error 'implementation-unbuildable
                     :implementation implementation
                     :condition c))))))))

#+sbcl
(defmethod build-in-directory/arch ((implementation sbcl) dir (arch (eql :emulated-x86)))
  (with-sbcl-build-setup (implementation dir)
    (invoke-logged-program "build-sbcl" (merge-pathnames #p"scripts/run-in-32bit" *base-dir*)
                           `("./make.sh" ,@*sbcl32-build-args*)
                           :environment `("CC=gcc-3.3"
                                          ,@(sb-ext:posix-environ)))
    implementation))

#+sbcl
(defmethod build-in-directory/arch ((implementation sbcl) dir (arch (eql :x86_64)))
  (with-sbcl-build-setup (implementation dir)
    (invoke-logged-program "build-sbcl" "make.sh" *sbcl64-build-args*
                           :environment `("CC=gcc-3.3"
                                          ,@(sb-ext:posix-environ)))
    implementation))

#+sbcl
(defmethod build-in-directory/arch ((implementation sbcl) dir arch)
  (with-sbcl-build-setup (implementation dir)
    (invoke-logged-program "build-sbcl" "make.sh" *sbcl-default-build-args*
                           :environment `("CC=gcc-3.3"
                                          ,@(sb-ext:posix-environ)))
    implementation))

(defun prepare-sbcl-environment ()
  (nconc 
   (remove-if (lambda (elt)
                (member elt '("SBCL_HOME" "LC_CTYPE" "LANGUAGE")
                        :test #'string-equal))
             #+sbcl(sb-ext:posix-environ)
             #-sbcl()
              :key (lambda (envl) (subseq envl 0 (position #\= envl))))
   (list "LC_CTYPE=C")))

(defun prepare-bench-sbcl-cmdline (impl arch shell-quote-p)
  `(,(format nil "~A" (shellquote (namestring (implementation-cached-file-name impl "sbcl")) shell-quote-p))
     "--core" ,(shellquote (namestring (implementation-cached-file-name impl "sbcl.core")) shell-quote-p)
     ,@(if (member arch '(:emulated-x86 :x86))
           (list "--dynamic-space-size" "2000"))
     "--userinit" "/dev/null" "--disable-debugger"
     "--boink-core-file" ,(shellquote (namestring (implementation-cached-file-name impl "sbcl.core")) shell-quote-p)
     "--boink-implementation-type" ,(shellquote (implementation-translated-mode impl) shell-quote-p)))

(defmethod run-benchmark/arch ((impl sbcl) (arch (eql :emulated-x86)))
  (with-unzipped-implementation-files impl
    (invoke-logged-program "bench-sbcl" (merge-pathnames #p"scripts/run-in-32bit" *base-dir*)
                           `("./run-sbcl.sh" ,@(prepare-bench-sbcl-cmdline impl arch t))
                           :environment (prepare-sbcl-environment))))

(defmethod run-benchmark/arch ((impl sbcl) (arch (eql :x86_64)))
  (with-unzipped-implementation-files impl
    (invoke-logged-program "bench-sbcl" "/usr/bin/env"
                           `("./run-sbcl.sh" ,@(prepare-bench-sbcl-cmdline impl arch nil))
                           :environment (prepare-sbcl-environment))))

(defmethod run-benchmark/arch ((impl sbcl) arch)
  (with-unzipped-implementation-files impl
    (invoke-logged-program "bench-sbcl" "/usr/bin/env"
                           `("./run-sbcl.sh" ,@(prepare-bench-sbcl-cmdline impl arch nil))
                           :environment (prepare-sbcl-environment))))

(defmethod implementation-required-files ((impl sbcl))
  (declare (ignore impl))
  (list #p"sbcl" #p"sbcl.core"))

(defmethod implementation-file-in-builddir ((impl sbcl) file-name)
  (declare (ignore impl))
  (cdr
   (assoc file-name
	  `((#p"sbcl" . #p"src/runtime/sbcl")
	    (#p"sbcl.core" . #p"output/sbcl.core"))
	  :test 'equal)))

;;; stuff for autobuilding on the main host. not for everybody, I think...

(defun build-sbcl-manual (impl dir)
  (handler-case
      (with-unzipped-implementation-files impl
        (invoke-logged-program "sbcl-build-manual"
                               (namestring (merge-pathnames #p"scripts/sbcl-build-manual"
                                                            *base-dir*))
                               `(,(namestring dir) "antifuchs"
                                  ,@(mapcar (lambda (f)
                                              (namestring
                                               (implementation-cached-file-name impl f)))
                                            (implementation-required-files impl)))
                               :environment `(,@(prepare-sbcl-environment))))
    (program-exited-abnormally ()
      (with-simple-restart (ignore "Ignore failed build of manual.")
        (error 'manual-unbuildable :implementation impl)))))

(defmethod build-in-directory :around ((impl sbcl) dir)
  "If this is the last revision, build the manual and report a
possible build failure to *SBCL-DEVELOPERS*."
  (if (and (not (has-next-directory-p impl dir))
           (equalp "baker" (machine-instance))) ;; only makes sense on one autobuild host
      (progn (call-next-method impl dir)
             (build-sbcl-manual impl dir)) 
      (call-next-method impl dir)))

;;; arch-tag: "3c5332be-00b6-11d9-a66e-000c76244c24"
