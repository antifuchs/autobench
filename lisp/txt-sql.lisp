(in-package #:autobench)

(defvar *version-translations*)

(defun release-p (vnum)          
  (if (search "pre" vnum)
      (<= (count #\. vnum) 1)
      (<= (count #\. vnum) 2)))

(defun translate-version (version)
  (when (string= "CMU Common Lisp" (first version))
    (setf (first version) "CMUCL"))
  (let ((v (assoc version *version-translations* :test #'equal)))
    (if v
        (cdr v)
        version)))

(defmacro ignore-pg-errors (&rest body)
  `(handler-case (progn ,@body)
     (pg::backend-error () nil)
     (pg::error-response () nil)))

(defun import-release-into-db (impl date)
  (ignore-pg-errors
   (pg-exec *dbconn* (format nil "insert into impl values ('~A', (select max(field_offset)+2 from impl))" (impl-name impl))))
  (ignore-pg-errors
   (pg-exec *dbconn* (format nil "insert into version (i_name, version, release_date, is_release) values ('~A', '~A', ~A, ~A)"
                             (impl-name impl) (impl-version impl) date
                             (if (release-p (impl-version impl)) "TRUE" "FALSE")))))

(defun read-benchmark-data (dir machine-name)
  (dolist (file (directory (merge-pathnames #p"CL-benchmark*.*" dir)))
    (with-open-file (s file :direction :input)
      (let ((*read-eval* nil)
            (*version-translations* (with-open-file (f *version-translations-file*
                                                       :if-does-not-exist :create
                                                       :direction :input)
                                      (let ((*read-eval* nil))
                                        (read f)))))
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
                       (pg-exec *dbconn* (format nil "insert into benchmark (name) values ('~A')" b-name)))))
                  (with-pg-transaction *dbconn*
                    (dolist (b benchmark)
                      (destructuring-bind (b-name r-secs u-secs &rest ignore) b
                        (declare (ignore r-secs ignore))
				  
                        (pg-exec *dbconn* (format nil "insert into result (date, v_name, v_version, b_name, m_name, seconds) ~
                                                       values (to_universal_time(~A::int4::abstime), '~A', '~A', '~A', '~A', ~f)"
                                                  mtime i-name i-version b-name machine-name u-secs))))))))
          (cl:end-of-file () nil))))
    (rename-file file (ensure-directories-exist
                       (merge-pathnames (make-pathname :name (pathname-name file))
                                        *archived-result-dir*)))))

;;; stand-alone stuff.
#+(or)
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
