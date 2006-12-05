(in-package #:autobench)

(defvar *version-translations*)
(defparameter *suppress-pg-errors-on-import* t
  "Parameter specifying whether or not to suppress duplicate key
  constraint and other errors when importing a build into the
  database. Required to be T in normal operation.")
(defparameter *suppress-pg-errors-on-import-benchmark* t
  "Parameter specifying whether or not to suppress duplicate key
  constraint and other errors when importing benchmarks into
  the database. Required to be T in normal operation.")


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
        ((eql (length "1.0.0")
              (mismatch "1.0.0" vnum))
         ;; the 1.0 release really should have been 1.0.0.
         "1.0")
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

(defmacro handle-pg-errors (((&rest condition-spec) &body handler-body) &body body)
  "Handle pg::backend-error and pg::error-response with the same handler-body."
  `(handler-case (progn ,@body)
     (pg::backend-error (,@condition-spec) ,@handler-body)
     (pg::error-response (,@condition-spec) ,@handler-body)))

(defmacro ignore-pg-errors (&rest body)
  `(handle-pg-errors ((c) (values nil c))
     ,@body))

(define-condition pg-error (error)
  ((cause :initarg :cause :accessor pg-error-cause)
   (statement :initarg :statement :accessor pg-error-statement))
  (:report (lambda (c s)
             (format s "Caught PG error ~A~%executing statement ~S."
                     (pg-error-cause c) (pg-error-statement c)))))

(defmacro optionally-suppressing-pg-errors ((condition &rest error-initargs) &body body)
  `(cond (,condition (ignore-pg-errors ,@body))
         (t (with-simple-restart (suppress-anyway "Condition would have been suppressed. Continue anyway.")
              (handle-pg-errors ((c) (error 'pg-error :cause c ,@error-initargs))
                ,@body)))))

(defun run-statement/suppressing-errors (suppress-errors-p statement-formatspec &rest statement-args)
  (let ((statement (apply #'format nil statement-formatspec statement-args)))
    (optionally-suppressing-pg-errors (suppress-errors-p :statement statement)
      (pg-exec (db-connection) statement))))

(defun import-release-from-dir (impl dir &key (suppress-errors-p *suppress-pg-errors-on-import*))
  (run-statement/suppressing-errors suppress-errors-p
                                    "insert into impl values ('~A', (select max(field_offset)+2 from impl))"
                                    (impl-name impl))
  (run-statement/suppressing-errors suppress-errors-p
                                    "insert into version (i_name, version, release_date, is_release, belongs_to_release) values ('~A', '~A', ~A, ~A, '~A')"
                                    (impl-name impl) (impl-version impl) (implementation-release-date impl dir)
                                    (if (release-p impl) "TRUE" "FALSE")
                                    (release-for-version impl))
  (run-statement/suppressing-errors suppress-errors-p
                                    "insert into build (v_name, v_version, mode) values ('~A', '~A', '~A')"
                                    (impl-name impl) (impl-version impl) (implementation-translated-mode impl)))

(defun read-benchmark-data (&optional (dir *base-result-dir*) (machine-name (machine-instance)))
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
                  ;; first, make sure the benchmark exists.
                  (dolist (b benchmark)
                    (destructuring-bind (b-name r-secs u-secs &rest ignore) b
                      (declare (ignore r-secs ignore u-secs))
                      (run-statement/suppressing-errors *suppress-pg-errors-on-import-benchmark*
                                                        "insert into benchmark (name) values ('~A')" b-name)))
                  ;; now insert the actual benchmark data - all in one transaction.
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