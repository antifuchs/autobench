(in-package :autobench)

(defclass cmucl (implementation)
     ((name :allocation :class :initform "CMUCL")))
(defclass cmucl-snapshot (cmucl)
     ())

(defmethod version-from-directory ((impl cmucl-snapshot) directory)
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (implementation-release-date impl directory))
    (declare (ignore second minute hour date day daylight-p zone))
    (format nil "~2,1,0,'0@A-~2,1,0,'0@A" year month)))

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
               (let ((dir (merge-pathnames (make-pathname :directory `(:relative ,(format nil "cmucl-~2,1,0,'0@A-~2,1,0,'0@A" year month)))
                                           *default-pathname-defaults*))
                     (tarfile-name (format nil *cmucl-snapshot-format* year month)))
                 (unless (probe-file dir)
                   (with-current-directory (ensure-directories-exist dir)
                     (handler-case
                         (progn
                           (invoke-logged-program "untar-cmucl" *tar-binary*
                                                  `("jxpvf" ,tarfile-name))
                           dir)
                       (program-exited-abnormally ()
                         nil))))))))
    (multiple-value-bind (c-second c-minute c-hour c-date c-month c-year c-day c-daylight-p c-zone) (get-decoded-time)
      (declare (ignore c-second c-minute c-hour c-date c-day c-daylight-p c-zone))
      (multiple-value-bind (second minute hour date month year day daylight-p zone)
          (decode-universal-time (implementation-release-date impl directory))
        (declare (ignore second minute hour date day daylight-p zone))
        (loop for next-month = (1+ (mod month 12)) then (1+ (mod next-month 12))
              for next-year = (if (= 1 next-month) (1+ year) year) then (if (= 1 next-month) (1+ next-year) next-year)
              for possible-dir = (try-snapshot next-month next-year)
              until (or (and (> next-month c-month) (>= next-year c-year))
                        (> next-year c-year))
              if (not (null possible-dir)) do (return possible-dir))))))

(defmethod implementation-release-date ((impl cmucl-snapshot) directory)
  (let ((stat (sb-posix:stat (merge-pathnames (make-pathname :directory '(:relative "bin")
                                                             :name "lisp")
                                              directory))))
    ;; XXX: Ew. I should factor unix-time-to-unversal-time from
    ;; araneida-glue in autobench-web in here.
    (+ 2208988800 (sb-posix:stat-mtime stat))))

;;; arch-tag: "96203cd3-bfff-425a-9da1-65670e870493"
