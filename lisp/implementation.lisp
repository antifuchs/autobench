(in-package :measure)

(defclass implementation ()
     ((version :accessor impl-version :initform nil :initarg :version)))

(define-condition implementation-unbuildable ()
  ((implementation :accessor unbuildable-implementation :initarg :implementation)))

;;; The implementation protocol - methods that have to be overridden
;;; when a new implementation is added

(defgeneric build-in-directory (implementation directory)
  (:documentation "Run the build for implementation
IMPLEMENTATION in DIRECTORY.  Possible side-effects: If the
method can determine (IMPL-VERSION IMPLEMENTATION), it sets the
slot in IMPLEMENTATION to the version."))

(defgeneric run-benchmark-for-version (implementation))

(defgeneric implementation-required-files (implementation)
  (:documentation "Returns the names of the files required to run
  IMPLEMENTATION."))

(defgeneric implementation-file-in-builddir (implementation file-name)
  (:documentation "Returns the pathname (relative to the build
  directory base) in which FILE-NAME resides after IMPLEMENTATION
  is finished building."))

(defgeneric implementation-runtime-file-name (implementation)
  (:documentation "Returns the runtime file's base name for
  IMPLEMENTATION."))

;;; Helper functions that methods in the implementation protocol may
;;; use and functions that our implementations may expect to be
;;; executed for them.

(defun impl-name (impl)
  (string-downcase (class-name impl)))

(defun implementation-cached-file-name (impl file-name)
  (make-pathname :directory (append *version-cache-dir*
				    (impl-version impl))
		 :name file-name))

(defun ensure-implementation-file-unpacked (impl file-name)
  (when (file-exists (implementation-cached-zip-file-name impl file-name))
    (unpack-file impl file-name))
  file-name)

(defmethod build-in-directory :around (impl dir)
  "After a finished build, move the required files for the
  version into the appropriate place in the build cache."
  (call-next-method impl dir)
  (loop for file in (implementation-required-files impl)
	do (rename-file (merge-pathnames (implementation-file-in-builddir impl file) dir)
			(ensure-directories-exist
			 (implementation-cached-file-name impl file)))))

;;; our own helper functions. Not really for public use.

(defun implementation-cached-zip-file-name (impl file-name)
  (let ((pn (implementation-cached-file-name impl file-name))))
  (setf (pathname-type pn) "gz")
  pn)

(defun unpack-file (impl file-name)
  ;; TODO: gunzip code
  nil)

(defun pack-file (impl file-name)
  ;; TODO: gzip code
  nil)

;;; arch-tag: "251ea0cc-ff5f-11d8-8b1b-000c76244c24"
