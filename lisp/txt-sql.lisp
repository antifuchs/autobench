(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'asdf)
  (require 'pg)
  (require 'sb-posix))

(defpackage #:sql-importer
  (:use #:cl #:pg))

(in-package #:sql-importer)

(defparameter *base-result-dir* #p"to-import/*/*.*")
(defparameter *machines* '("walrus.boinkor.net"))
(defvar *version-translations* (with-open-file (f #p"version-translations.lisp-expr"
						:if-does-not-exist :create
 						:direction :input)
                                 (let ((*read-eval* nil))
                                   (read f))))

(defun implementation-version (impl)
  "Return the implementation's version, when passed as a
  string. This is a silly and unneccessary function. May god
  forgive me."
  (declare (optimize (speed 0))
	   (type base-string impl))
  (subseq impl
	  (1+ (or (position #\Space impl :from-end t)
		  -1))))

(defun implementation-name (impl)
  "Return the implementation's name, when passed as a
  string. This is a silly and unneccessary function. May god
  forgive me."
  (declare (optimize (speed 0))
	   (type base-string impl))
  (subseq impl
	  0
	  (or (position #\Space impl :from-end t)
	      0)))

(defun translate-version (version-name)
  (let ((v (assoc version-name *version-translations* :test #'equal)))
    (if v
        (cdr v)
        version-name)))

(defmacro ignore-pg-errors (&rest body)
  `(handler-case (progn ,@body)
     (pg::backend-error () nil)))

(defun populate-results (dir machine-name)
  (with-open-file (f #p"/home/asf/tmp/populate.sql" :direction :output :if-exists :supersede)
    (dolist (file (directory (merge-pathnames #p"CL-benchmark*.*" dir)))
      (with-open-file (s file :direction :input)
	(let ((*read-eval* nil))
	  (handler-case (destructuring-bind (version-s &rest benchmark) (read s :eof-error-p nil)
			  (let* ((i-version (implementation-version version-s))
				 (i-name (implementation-name version-s))
				 (stat (sb-posix:stat file))
				 (mtime (sb-posix::stat-mtime stat)))
			    (dolist (b benchmark)
			      (destructuring-bind (b-name r-secs u-secs &rest ignore) b
				(declare (ignore r-secs ignore))
				  
				(format f "insert into benchmark values ('~A');~%" b-name)
				(format f "insert into version values ('~A', '~A');~%" i-name i-version)
				(format f "insert into result values (~A::int4::abstime, '~A', '~A', '~A', '~A', ~f);~%"
					mtime i-name i-version b-name machine-name u-secs)))))
	    (cl:end-of-file () nil)))))))

(defun read-benchmark-data (dir machine-name)
  (with-pg-connection (c "sbclarch" "sbclarch" :host "localhost" :password "Sahr6poh")
    (dolist (file (directory (merge-pathnames #p"CL-benchmark*.*" dir)))
      (with-open-file (s file :direction :input)
	(let ((*read-eval* nil))
	  (handler-case
	      (destructuring-bind (version-s &rest benchmark) (read s :eof-error-p nil)
		(let* ((i-version (implementation-version (translate-version version-s)))
		       (i-name (implementation-name (translate-version version-s)))
		       (stat (sb-posix:stat file))
		       (mtime (sb-posix::stat-mtime stat)))
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
