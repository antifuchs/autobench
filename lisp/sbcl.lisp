(in-package :measure)

(defclass sbcl (implementation) ())

(defmethod version-from-directory ((implementation sbcl) directory)
  (declare (ignore impl))
  (with-open-file (f #p"version.lisp-expr" :direction :input)
    (read f)))

(defmethod build-in-directory ((implementation sbcl) directory)
  (let* ((*default-pathname-defaults* directory)
	 (*read-eval* nil))
    (let ((process (invoke-logged-program "build-sbcl" #p"make.sh" *sbcl-build-args*)))
      (sb-ext:process-wait process)
      (unless (zerop (sb-ext:process-exit-code process))
	(error 'implementation-unbuildable
	       :implementation implementation)))
    implementation))

(defmethod run-benchmark ((impl sbcl))
  (with-unzipped-implementation-files impl
    (invoke-logged-program "bench-sbcl" "/usr/bin/env" '("bash" "run-sbcl.sh")
			   :environment `(,(format nil "SBCL=~A" (namestring (implementation-cached-file-name impl "sbcl")))
					   ,(format nil "SBCL_OPT=--core ~A --userinit /dev/null --disable-debugger"
						    (namestring (implementation-cached-file-name impl "sbcl.core")))
					   ,@(remove "SBCL_HOME" (sb-ext:posix-environ)
						     :test #'string-equal
						     :key (lambda (envl)
							    (subseq envl 0 (position #\= envl))))))))

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


;;; arch-tag: "3c5332be-00b6-11d9-a66e-000c76244c24"
