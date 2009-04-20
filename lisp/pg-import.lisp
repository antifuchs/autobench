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

(defgeneric release-for-version (impl)
  (:method ((impl sbcl))
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
  (:method ((impl clisp))
    (let ((vnum (impl-version impl)))
      ;; release versions look like clisp.2.25-2001-03-15
      ;; non-release versions:      clisp.2.41-2006-10-13-nn-g0126e2a
      (labels ((match-nth (n elt)
                 (lambda (element)
                   (cond ((and (eql element elt)
                               (zerop n))
                          t)
                         ((eql element elt)
                          (decf n)
                          nil)))))
        (cond
          ((= (count #\- vnum) 3) vnum)
          (t (subseq vnum 0 (position-if (match-nth 1 #\-) vnum  :from-end t))))))))

(defun release-p (impl)
  (string-equal (release-for-version impl) (impl-version impl)))

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
     (postmodern:database-error (,@condition-spec) ,@handler-body)))

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
      (query statement))))

(defun import-release-from-dir (impl dir)
  (setf (impl-version impl) (or (impl-version impl)
                                (version-from-directory impl dir)))
  (with-db-connection ()
    (with-transaction (insert-release)
      (handler-case
          (with-savepoint one-implementation
            (query (:insert-into 'implementations :set
                                 'implementation-name (impl-name impl)
                                 'mode (prin1-to-string (impl-mode impl))
                                 'machine-name (machine-instance))))
        (cl-postgres-error:unique-violation ()
          nil))
      (let ((version-id nil))
        (handler-case
            (with-savepoint make-version
              (setf version-id
                    (query (:insert-into 'versions :set
                                         'implementation-name (impl-name impl) 'version-number (impl-version impl)
                                         'release-date (implementation-release-date impl dir)
                                         'is-release (release-p impl)
                                         'belongs-to-release (release-for-version impl)
                                         'version-code (or (implementation-version-code impl dir)
                                                           :null)
                                         :returning 'version-id)
                           :single)))
          (cl-postgres-error:unique-violation ()
            (with-savepoint get-version
              (setf version-id
                    (query (:select 'version-id :from 'versions
                                    :where (:and (:= 'implementation-name
                                                     (impl-name impl))
                                                 (:= 'version-number
                                                     (impl-version impl))))
                           :single)))))
        (assert (not (null version-id)))
        (handler-case
            (with-savepoint one-build
              (query (:insert-into 'builds :set
                                   'version-id version-id
                                   'mode (implementation-translated-mode impl))))
          (cl-postgres-error:unique-violation ()
            nil))))))

(defun process-benchmark-data (&optional (dir *base-result-dir*)
                               (machine-name (machine-instance)))
  (with-db-connection ()
    (dolist (file (directory (merge-pathnames #p"CL-benchmark*.*" dir)))
      (catch 'next-benchmark
        (multiple-value-bind (i-name version mode benchmark)
            (read-benchmark-data file)
          (unless version
            ;; No results
            (throw 'next-benchmark nil))
          (let ((mtime (simple-date:universal-time-to-timestamp (file-write-date file)))
                (version-id (query (:select 'version-id :from 'versions :where
                                            (:and (:= 'implementation-name i-name)
                                                  (:= 'version-number version)))
                                   :single!)))
            ;; First, make sure the benchmarks exist: Since we're in a txn,
            ;; we make one savepoint per benchmark and roll back if a benchmark
            ;; already exists.
            (with-transaction (insert-results)
              (dolist (b benchmark)
                (destructuring-bind (b-name r-secs u-secs &rest ignore) b
                  (declare (ignore r-secs ignore u-secs))
                  (ignore-errors
                    (with-savepoint one-bm
                      (query (:insert-into 'benchmarks :set
                                           'benchmark-name b-name
                                           'benchmark-version +benchmark-version+))))))
              ;; Now insert the actual benchmark data
              (dolist (b benchmark)
                (destructuring-bind (b-name r-secs u-secs &rest ignore) b
                  (declare (ignore r-secs ignore))
                  (query
                   (:insert-into 'results :set
                                 'result-date mtime
                                 'version-id version-id
                                 'mode mode
                                 'benchmark-name b-name
                                 'benchmark-version +benchmark-version+
                                 'machine-name machine-name
                                 'seconds u-secs))))))))
      (rename-file file (ensure-directories-exist
                         (merge-pathnames
                          (make-pathname :name (pathname-name file))
                          *archived-result-dir*))))))

(defun read-benchmark-data (file)
  (with-open-file (s file :direction :input)
    (let ((*read-eval* nil)
          (*version-translations* (with-open-file (f *version-translations-file*
                                                     :if-does-not-exist :create
                                                     :direction :input)
                                    (let ((*read-eval* nil))
                                      (read f)))))
      (handler-case
          (destructuring-bind (impl mode version &rest benchmark)
              (read s :eof-error-p nil)
            (destructuring-bind (i-name version)
                (translate-version (list impl version))
              (values i-name version mode benchmark)))
        (cl:end-of-file () nil)))))