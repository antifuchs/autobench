(in-package #:measure)

(defvar *version-translations* (with-open-file (f *version-translations-file*
						:if-does-not-exist :create
 						:direction :input)
                                 (let ((*read-eval* nil))
                                   (read f))))

(defun translate-version (version)
  (let ((v (assoc version *version-translations* :test #'equal)))
    (if v
        (cdr v)
        version)))

(defmacro ignore-pg-errors (&rest body)
  `(handler-case (progn ,@body)
     (pg::backend-error () nil)))

(defun read-benchmark-data (dir machine-name)
  (with-pg-connection (c "sbclarch" "sbclarch" :host "localhost" :password "Sahr6poh")
    (dolist (file (directory (merge-pathnames #p"CL-benchmark*.*" dir)))
      (with-open-file (s file :direction :input)
	(let ((*read-eval* nil))
	  (handler-case
	      (destructuring-bind (impl version &rest benchmark) (read s :eof-error-p nil)
		(destructuring-bind (i-name i-version) (translate-version (list impl version))
		  (let* ((stat (sb-posix:stat file))
			 (mtime (sb-posix::stat-mtime stat)))
		    ;; first, make sure the benchmark exists; we don't
		    ;; do this below, because we insert all values in
		    ;; one transaction. benchmarks should be inserted
		    ;; regardless of psql errors.
		    (dolist (b benchmark)
		      (destructuring-bind (b-name r-secs u-secs &rest ignore) b
			(declare (ignore r-secs ignore u-secs))
			(ignore-pg-errors
			 (pg-exec c (format nil "insert into benchmark values ('~A')" b-name)))))
		    (with-pg-transaction c
		      (dolist (b benchmark)
			(destructuring-bind (b-name r-secs u-secs &rest ignore) b
			  (declare (ignore r-secs ignore))
				  
			  (pg-exec c (format nil "insert into result values (~A::int4::abstime, '~A', '~A', '~A', '~A', ~f)"
					     mtime i-name i-version b-name machine-name u-secs)))))))
		(cl:end-of-file () nil)))))))

;;; stand-alone stuff.
(dolist (machine-dir (remove-duplicates
		      (mapcar (lambda (file)
				(list (first (last (pathname-directory file)))
				      (pathname-directory file)))
			      (directory *base-result-dir*))
		      :test #'equal))
  (destructuring-bind (machine dir) machine-dir
    (read-benchmark-data (make-pathname :directory dir)
			 machine)))

;;; arch-tag: "9a7ec82b-ff30-11d8-8b1b-000c76244c24"
