(in-package :autobench)

(defclass sbcl (implementation)
     ((name :allocation :class :initform "SBCL")))

(defclass sbcl-character (sbcl)
     ((name :allocation :class :initform "SBCL-character")))

(defmethod version-from-directory ((impl sbcl) directory)
  (declare (ignore impl))
  (let ((*read-eval* nil))
    (with-open-file (f #p"version.lisp-expr" :direction :input)
      (read f))))

(defmethod build-in-directory ((implementation sbcl) directory)
  (with-current-directory directory
    (handler-case (invoke-logged-program "build-sbcl" #p"make.sh" *sbcl-build-args*)
      (program-exited-abnormally ()
        (error 'implementation-unbuildable
               :implementation implementation)))
    implementation))

(defun prepare-sbcl-environment ()
  (remove "SBCL_HOME" (sb-ext:posix-environ)
          :test #'string-equal
          :key (lambda (envl) (subseq envl 0 (position #\= envl)))))

(defmethod run-benchmark ((impl sbcl))
  (with-unzipped-implementation-files impl
    (invoke-logged-program "bench-sbcl" "/usr/bin/env" '("bash" "run-sbcl.sh")
			   :environment `(,(format nil "SBCL=~A" (namestring (implementation-cached-file-name impl "sbcl")))
					  ,(format nil "SBCL_OPT=--core ~A --userinit /dev/null --disable-debugger --boink-core-file ~A"
						   (namestring (implementation-cached-file-name impl "sbcl.core"))
						   (namestring (implementation-cached-file-name impl "sbcl.core")))
					  ,@(prepare-sbcl-environment)))))

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
  (with-input-from-program (missing *tla-binary* "missing" "--dir" (namestring directory))
    (with-input-from-program (version *tla-binary* "tree-version" (namestring directory))
      (let* ((version (read-line version))
	     (next-rev (read-line missing nil nil))
	     (revision-spec (format nil "~A--~A" version next-rev)))
	(when next-rev
	  (invoke-logged-program "tla-sbcl-update" *tla-binary*
				 `("update" "--dir" ,(namestring directory) ,revision-spec))
	  directory)))))

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
           (equalp "walrus.boinkor.net" (machine-instance))) ;; only makes sense on walrus
      (handler-case (progn (call-next-method impl dir)
                           (build-manual impl dir))
        (implementation-unbuildable (e)
          (send-mail-to *sbcl-developers* (format nil "Can't build ~A" (unbuildable-implementation e))))
        (program-exited-abnormally (e)
          (send-mail-to *sbcl-developers* (format nil "Can't build manual for ~A: ~A" impl e))))
      (call-next-method impl dir)))

;;; arch-tag: "3c5332be-00b6-11d9-a66e-000c76244c24"
