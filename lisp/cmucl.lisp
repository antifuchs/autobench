(in-package :autobench)

(defclass cmucl (implementation)
     ((name :allocation :class :initform "CMUCL")))
(defclass cmucl-snapshot (cmucl)
     ())

(defmethod version-from-directory ((impl cmucl-snapshot) directory)
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (implementation-release-date impl directory))
    (declare (ignore second minute hour date day daylight-p zone))
    (format nil "~A-~A" year month)))

(defmethod build-in-directory ((implementation cmucl-snapshot) directory)
  (with-current-directory directory
    (unless (probe-file #p"bin/lisp")
      (error 'implementation-unbuildable
             :implementation implementation))
    implementation))

(defmethod run-benchmark ((impl cmucl))
  (with-unzipped-implementation-files impl
    (invoke-logged-program "bench-cmucl" "/usr/bin/env" '("bash" "run-cmucl.sh")
			   :environment `(,(format nil "CMUCL=~A" (namestring (implementation-cached-file-name impl "lisp")))
					  ,(format nil "CMUCLOPT=-batch -core ~A --boink-core-file ~A"
						   (namestring (implementation-cached-file-name impl "lisp.core"))
						   (namestring (implementation-cached-file-name impl "lisp.core")))
					  ,@(sb-ext:posix-environ)))))

(defmethod implementation-required-files ((impl cmucl))
  (declare (ignore impl))
  (list #p"lisp" #p"lisp.core"))

(defmethod implementation-file-in-builddir ((impl cmucl) file-name)
  (declare (ignore impl))
  (cdr
   (assoc file-name
	  `((#p"lisp" . #p"bin/lisp")
	    (#p"lisp.core" . #p"lib/cmucl/lib/lisp.core"))
	  :test 'equal)))

(defmethod next-directory ((impl cmucl-snapshot) directory)
  (labels ((try-snapshot (month year)
             (with-current-directory (merge-pathnames (make-pathname :directory '(:relative :up))
                                                      directory)
               (let ((dir (merge-pathnames (make-pathname :directory `(:relative ,(format nil "cmucl-~A-~A" year month)))
                                           *default-pathname-defaults*))
                     (tarfile-name (format nil *cmucl-snapshot-format* year month)))
                 (with-current-directory (ensure-directories-exist dir)
                   (handler-case
                       (progn
                         (invoke-logged-program "untar-cmucl" *tar-binary*
                                                `("jxpvf" ,tarfile-name))
                         dir)
                     (program-exited-abnormally ()
                       nil)))))))
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (implementation-release-date impl directory))
      (loop for next-month = (1+ (mod month 12))
            for next-year = (if (= 1 next-month) (1+ year) year)
            for possible-dir = (try-snapshot next-month next-year)
            if (not (null possible-dir)) do (return possible-dir)))))

(defmethod implementation-release-date ((impl cmucl-snapshot) directory)
  (let ((stat (sb-posix:stat (merge-pathnames (make-pathname :directory '(:relative "bin")
                                                             :name "lisp")
                                              directory))))
    (sb-posix:stat-mtime stat)))

;;; arch-tag: "96203cd3-bfff-425a-9da1-65670e870493"
