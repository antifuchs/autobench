(in-package :measure)

(defclass implementation ()
     ((version :accessor impl-version :initform nil :initarg :version)))

(define-condition implementation-unbuildable ()
  ((implementation :accessor unbuildable-implementation :initarg :implementation)))

;;; The implementation protocol - methods that have to be overridden
;;; when a new implementation is added

(defgeneric build-in-directory (implementation directory)
  (:documentation "Run the build for implementation
IMPLEMENTATION in DIRECTORY.

Signals a condition of type IMPLEMENTATION-UNBUILDABLE if the
implementation can not be built.

Returns a new imlementation instance with the slot VERSION set to
the built implementation's version."))

(defgeneric run-benchmark (implementation))

(defgeneric implementation-required-files (implementation)
  (:documentation "Returns the names of the files required to run
  IMPLEMENTATION."))

(defgeneric implementation-file-in-builddir (implementation file-name)
  (:documentation "Returns the pathname (relative to the build
  directory base) in which FILE-NAME resides after IMPLEMENTATION
  is finished building."))

;;; Helper functions that methods in the implementation protocol may
;;; use and functions that our implementations may expect to be
;;; executed for them.

(defun impl-name (impl)
  (string-downcase (class-name (class-of impl))))

(defun implementation-cached-file-name (impl file-name)
  (make-pathname :directory (append (pathname-directory *version-cache-dir*)
				    (list (impl-name impl) (impl-version impl)))
		 :name (pathname-name file-name)
		 :type (pathname-type file-name)))

(defmacro with-unzipped-implementation-files (implementation &body body)
  (with-gensyms (impl-files impl file)
    `(let* ((,impl ,implementation)
	    (,impl-files (implementation-required-files ,impl)))
       (unwind-protect (progn
			 (loop for ,file in ,impl-files
			       do (ensure-implementation-file-unpacked ,impl ,file))
			 ,@body)
	 (loop for ,file in ,impl-files
	       do (pack-file ,impl (implementation-cached-file-name ,impl ,file)))))))

(defun invoke-logged-program (step-name program args &key (environment (sb-ext:posix-environ)))
  (multiple-value-bind (second minute hour date month year day daylight-p zone) (get-decoded-time)
      (declare (ignore day daylight-p zone))
      (let ((output-pathname (merge-pathnames
			      (make-pathname
			       :name (format nil "~A_~4,1,0,'0@A-~2,1,0,'0@A-~2,1,0,'0@AT~2,1,0,'0@A:~2,1,0,'0@A:~2,1,0,'0@A"
					     step-name year month date hour minute second))
			      *log-directory*)))
	(ensure-directories-exist output-pathname)
	(sb-ext:run-program program args
			    :input nil
			    :environment environment
			    :output output-pathname))))

(defmethod build-in-directory :around (impl dir)
  "After a finished build, move the required files for the
  version into the appropriate place in the build cache."
  (sb-posix:chdir dir)
  (unwind-protect (let ((impl (call-next-method impl dir)))
		    (loop for file in (implementation-required-files impl)
			  do (rename-file (merge-pathnames (implementation-file-in-builddir impl file) dir)
					  (ensure-directories-exist
					   (implementation-cached-file-name impl file))))
		    impl)
    (sb-posix:chdir *base-dir*)))

(defun ensure-implementation-file-unpacked (impl file-name)
  (when (probe-file (implementation-cached-zip-file-name impl file-name))
    (unpack-file impl file-name))
  file-name)

;;; our own helper functions. Not really for public use.

(defun implementation-cached-zip-file-name (impl file-name)
  (let ((pn (implementation-cached-file-name impl file-name)))
    (make-pathname :directory (pathname-directory pn)
		   ;; XXX: suboptimal. but I can't think of a better way right now.
		   :name (format nil "~A~@[.~A~]" (pathname-name pn) (pathname-type pn))
		   :type "gz")))

(defun unpack-file (impl file-name)
  (declare (ignorable impl file-name))
  ;; TODO: gunzip code
  nil)

(defun pack-file (impl file-name)
  (declare (ignorable impl file-name))
  ;; TODO: gzip code
  nil)

;;; arch-tag: "251ea0cc-ff5f-11d8-8b1b-000c76244c24"
