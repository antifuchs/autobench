(in-package #:autobench)

(defvar *version-translations*)


;;; the release-p methods should soon expire...
(defmethod release-p ((impl sbcl))
  (let ((vnum (impl-version impl)))
    (if (search "pre" vnum)
        (<= (count #\. vnum) 1)
        (<= (count #\. vnum) 2))))

(defmethod release-p ((impl clisp))
  (let ((vnum (impl-version impl)))
    ;; release versions look like clisp.2.25-2001-03-15
    (= (count #\- vnum) 3)))


;;; ...and be replaced by these:
(defmethod release-for-version ((impl sbcl))
  (let ((vnum (impl-version impl)))
    (labels ((strip-last-dotnumber (vnum)
               (subseq vnum 0 (position #\. vnum :from-end t))))
      (cond
        ((search "pre" vnum)
         ;; Pre versions belong to their releases
         (concatenate 'string
                      (strip-last-dotnumber
                       (substitute "" "pre" vnum))
                      ".0"))
        ((<= (count #\. vnum) 2)
         ;; Releases belong to themselves
         vnum)
        (t
         ;; All other candidates get their last component stripped
         (strip-last-dotnumber vnum))))))

(defmethod release-for-version ((impl clisp))
  (let ((vnum (impl-version impl)))
    ;; release versions look like clisp.2.25-2001-03-15
    ;; non-release versions:      clisp.2.41-2006-10-13-g0126e2a
    (cond
      ((= (count #\- vnum) 3) vnum)
      (t (subseq vnum 0 (position #\- vnum :from-end t))))))


(defun translate-version (version)
  (let ((version (copy-list version)))
    (when (string= "CMU Common Lisp" (first version))
      (setf (first version) "CMUCL"))
    (when (string= "CLISP" (first version))
      (setf (second version) (subseq (second version)
                                     (mismatch (second version) "clisp."))))
    (let ((v (assoc version *version-translations* :test #'equal)))
      (if v
          (cdr v)
          version))))

(defmacro ignore-pg-errors (&rest body)
  `(handler-case (progn ,@body)
     (pg::backend-error () nil)
     (pg::error-response () nil)))

(defun import-release-into-db (impl date mode)
  (ignore-pg-errors
   (pg-exec (db-connection) (format nil "insert into impl values ('~A', (select max(field_offset)+2 from impl))" (impl-name impl))))
  (ignore-pg-errors
   (pg-exec (db-connection) (format nil "insert into version (i_name, version, release_date, is_release, belongs_to_release) values ('~A', '~A', ~A, ~A, '~A')"
                             (impl-name impl) (impl-version impl) date
                             (if (release-p impl) "TRUE" "FALSE")
                             (release-for-version impl))))
  (ignore-pg-errors
   (pg-exec (db-connection) (format nil "insert into build (v_name, v_version, mode) values ('~A', '~A', '~A')"
                             (impl-name impl) (impl-version impl) mode))))

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
            (destructuring-bind (impl mode version &rest benchmark) (read s :eof-error-p nil)
              (destructuring-bind (i-name version) (translate-version (list impl version))
                (let ((mtime (file-write-date file)))
                  ;; first, make sure the benchmark exists; we don't
                  ;; do this below, because we insert all values in
                  ;; one transaction. benchmarks should be inserted
                  ;; regardless of psql errors.
                  (dolist (b benchmark)
                    (destructuring-bind (b-name r-secs u-secs &rest ignore) b
                      (declare (ignore r-secs ignore u-secs))
                      (ignore-pg-errors
                       (pg-exec (db-connection) (format nil "insert into benchmark (name) values ('~A')" b-name)))))
                  (with-pg-transaction (db-connection)
                    (dolist (b benchmark)
                      (destructuring-bind (b-name r-secs u-secs &rest ignore) b
                        (declare (ignore r-secs ignore))
				  
                        (pg-exec (db-connection) (format nil "insert into result (date, v_name, v_version, mode, b_name, m_name, seconds) ~
                                                       values (~A, '~A', '~A', '~A', '~A', '~A', ~f)"
                                                   mtime i-name version mode b-name machine-name u-secs))))))))
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
