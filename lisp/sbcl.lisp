(in-package :autobench)

(defclass sbcl (implementation architecture-mixin)
     ((name :allocation :class :initform "SBCL")))

(defmethod version-from-directory ((impl sbcl) directory)
  (declare (ignore impl))
  (let ((*read-eval* nil))
    (with-open-file (f #p"version.lisp-expr" :direction :input)
      (read f))))

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

(defun cleanup-build-dir ()
  (when (probe-file #p"tools-for-build/grovel-headers")
    (delete-file #p"tools-for-build/grovel-headers")))

(defmacro with-sbcl-build-setup ((implementation directory) &body body)
  (let ((arch (gensym))
        (features (gensym)))
   `(with-current-directory ,directory
      (destructuring-bind (&key ((:arch ,arch)) ((:features ,features))) (impl-mode ,implementation)
        (declare (ignorable ,arch))
        (cleanup-build-dir)
        (with-customize-target-features ,features
          (handler-case (progn ,@body)
            (program-exited-abnormally ()
              (error 'implementation-unbuildable
                     :implementation implementation))))))))

(defmethod build-in-directory/arch ((implementation sbcl) dir (arch (eql :emulated-x86)))
  (with-sbcl-build-setup (implementation dir)
    (invoke-logged-program "build-sbcl" (merge-pathnames #p"scripts/run-in-32bit" *base-dir*)
                           `("./make.sh" ,@*sbcl32-build-args*)
                           :environment `("CC=gcc-2.95"
                                          ,@(sb-ext:posix-environ)))
    implementation))

(defmethod build-in-directory/arch ((implementation sbcl) dir (arch (eql :x86_64)))
  (with-sbcl-build-setup (implementation dir)
    (invoke-logged-program "build-sbcl" "make.sh" *sbcl64-build-args*
                           :environment `("CC=gcc-3.3"
                                          ,@(sb-ext:posix-environ)))
    implementation))

(defmethod build-in-directory/arch ((implementation sbcl) dir arch)
  (with-sbcl-build-setup (implementation dir)
    (invoke-logged-program "build-sbcl" "make.sh" *sbcl-default-build-args*
                           :environment `("CC=gcc-3.3"
                                          ,@(sb-ext:posix-environ)))
    implementation))

(defun prepare-sbcl-environment ()
  (remove "SBCL_HOME" (sb-ext:posix-environ)
          :test #'string-equal
          :key (lambda (envl) (subseq envl 0 (position #\= envl)))))

(defun shellquote (arg quote-p)
  (if quote-p
      (format nil "'~A'" arg)
      arg))

(defun prepare-bench-sbcl-cmdline (impl shell-quote-p)
  (list (format nil "~A" (shellquote (namestring (implementation-cached-file-name impl "sbcl")) shell-quote-p))
        "--core" (shellquote (namestring (implementation-cached-file-name impl "sbcl.core")) shell-quote-p)
        "--userinit" "/dev/null" "--disable-debugger"
        "--boink-core-file" (shellquote (namestring (implementation-cached-file-name impl "sbcl.core")) shell-quote-p)
        "--boink-implementation-type" (shellquote (implementation-translated-mode impl) shell-quote-p)))

(defmethod run-benchmark/arch ((impl sbcl) (arch (eql :emulated-x86)))
  (with-unzipped-implementation-files impl
    (invoke-logged-program "bench-sbcl" (merge-pathnames #p"scripts/run-in-32bit" *base-dir*)
                           `("./run-sbcl.sh" ,@(prepare-bench-sbcl-cmdline impl t))
                           :environment (prepare-sbcl-environment))))

(defmethod run-benchmark/arch ((impl sbcl) (arch (eql :x86_64)))
  (with-unzipped-implementation-files impl
    (invoke-logged-program "bench-sbcl" "/usr/bin/env"
                           `("./run-sbcl.sh" ,@(prepare-bench-sbcl-cmdline impl nil))
                           :environment (prepare-sbcl-environment))))

(defmethod run-benchmark/arch ((impl sbcl) arch)
  (with-unzipped-implementation-files impl
    (invoke-logged-program "bench-sbcl" "/usr/bin/env"
                           `("./run-sbcl.sh" ,@(prepare-bench-sbcl-cmdline impl nil))
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

(defmethod next-directory ((impl sbcl) directory)
  (with-input-from-program (missing *tla-binary* "missing" "-f" "--dir" (namestring directory))
    (let* ((next-rev (read-line missing nil nil)))
      (when next-rev
        (handler-case
            (invoke-logged-program "baz-sbcl-update" *tla-binary*
                                `("update" "--dir" ,(namestring directory) ,next-rev))
          (program-exited-abnormally ()
            ;; baz generated a conflict, maybe.
            (invoke-logged-program "rm-sbcl-build-dir" "/bin/rm"
                                   `("-rf" ,(namestring directory)))
            (invoke-logged-program "baz-sbcl-get" *tla-binary*
                                `("get" ,next-rev ,(namestring directory)))))
        directory))))

(defmethod implementation-release-date ((impl sbcl) directory)
  (with-input-from-program (last-revision *tla-binary* "logs" "-r" "-d" (namestring directory))
    (with-input-from-program (log *tla-binary* "cat-log" "-d" (namestring directory) (read-line last-revision))
      (let ((date-line (iterate (for line in-stream log using #'read-line)
                                (finding line such-that (and (= (mismatch line "Date: ") 6)
                                                             (not (null (find #\/ line))))))))
        (net.telent.date:parse-time date-line :start 6)))))

;;; stuff for autobuilding on walrus. not for everybody, I think...

(defun last-version-p (directory)
  (with-input-from-program (missing *tla-binary* "missing" "--dir" (namestring directory))
    (null (read-line missing nil nil))))

(defun send-mail-to (address subject)
  ;; TODO: send mail. No idea how to do that, yet
  (format t "Would have sent mail to ~s with subject ~S~%" address subject))

(defun build-manual (impl dir)
  (with-unzipped-implementation-files impl
    (invoke-logged-program "sbcl-build-manual" (namestring (merge-pathnames #p"scripts/sbcl-build-manual" *base-dir*))
                           `(,(namestring dir) "antifuchs"
                              ,@(mapcar (lambda (f) (namestring (implementation-cached-file-name impl f)))
                                       (implementation-required-files impl)))
                           :environment `(,@(prepare-sbcl-environment)))))

(defmethod build-in-directory :around ((impl sbcl) dir)
  "If this is the last revision, build the manual and report a
possible build failure to *SBCL-DEVELOPERS*."
  (if (and (last-version-p dir)
           (equalp "baker" (machine-instance))) ;; only makes sense on one autobuild host
      (handler-case (progn (call-next-method impl dir)
                           (build-manual impl dir))
        (implementation-unbuildable (e)
          (send-mail-to *sbcl-developers* (format nil "Can't build ~A" (unbuildable-implementation e))))
        (program-exited-abnormally (e)
          (send-mail-to *sbcl-developers* (format nil "Can't build manual for ~A: ~A" impl e))))
      (call-next-method impl dir)))

;;; arch-tag: "3c5332be-00b6-11d9-a66e-000c76244c24"
