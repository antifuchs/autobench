(in-package :autobench)

(defclass implementation ()
     ((version :accessor impl-version :initform nil :initarg :version)
      (name :accessor impl-name :allocation :class :initform nil)
      (mode :accessor impl-mode :allocation :instance :initform nil :initarg :mode)))

(define-condition implementation-unbuildable ()
  ((implementation :accessor unbuildable-implementation :initarg :implementation)))

(define-condition manual-unbuildable ()
  ((implementation :accessor unbuildable-implementation :initarg :implementation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The implementation protocol - methods that have to be overridden
;;; when a new implementation is added
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; With methods for these, you can use the auto-benchmarker on
;;; implementations whose files are in the cache directory (and whose
;;; versions have entries in the database already):

(defgeneric run-benchmark (implementation))

(defgeneric implementation-required-files (implementation)
  (:documentation "Returns the names of the files required to run
  IMPLEMENTATION."))

;;; With methods for these, you can use the autobuilder to build
;;; single implementation versions, and automatically put records for
;;; them in the database

(defgeneric version-from-directory (implementation directory)
  (:documentation "Returns the version of the implementation in DIRECTORY."))

(defgeneric clean-directory (implementation directory)
  (:documentation "Prepares DIRECTORY for a \"clean\" build.")
  (:method (impl directory)
    (declare (ignore impl directory))))

(defgeneric build-in-directory (implementation directory)
  (:documentation "Run the build for implementation
IMPLEMENTATION in DIRECTORY.

Signals a condition of type IMPLEMENTATION-UNBUILDABLE if the
implementation can not be built."))

(defgeneric implementation-file-in-builddir (implementation file-name)
  (:documentation "Returns the pathname (relative to the build
  directory base) in which FILE-NAME resides after IMPLEMENTATION
  is finished building.")
  (:method :around (impl pathname)
    (assert (member pathname (implementation-required-files impl)
                    :test #'equal)
            (pathname)
            "~A is not a required file of implementation ~A"
            pathname impl)
    (call-next-method)))

(defgeneric implementation-release-date (implementation directory)
  (:documentation "Returns the UNIVERSAL-DATE on which
IMPLEMENTATION (source tree in DIRECTORY) was released."))

;;; With a method for this, you can use the autobuilder to build many
;;; implementation versions in a row, for example every SBCL
;;; changeset, or from a set of release version directories

(defgeneric next-directory (implementation directory)
  (:documentation "Returns the directory in which the next
version of IMPLEMENTATION can be found. It is also permitted to
modify DIRECTORY and return it.

To indicate that there is no next implementation available,
return NIL."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Architecture Protocol Some implementations need help when
;;; running/building on different architectures. The methods on this
;;; mixin use the MODE slot of an implementation to dispatch on the
;;; architecture contained in it.

(defclass architecture-mixin () ())

(defgeneric build-in-directory/arch (implementation directory architecture)
  (:documentation "Analogous to build-in-directory, build implementation in directory.
Allows specialization on ARCH."))

(defgeneric run-benchmark/arch (implementation architecture)
  (:documentation "Analogous to run-benchmark, run benchmarks for implementation.
Allows specialization on ARCH."))

(defmethod build-in-directory ((implementation architecture-mixin) directory)
  (destructuring-bind (&key arch &allow-other-keys) (impl-mode implementation)
    (build-in-directory/arch implementation directory arch)))

(defmethod run-benchmark ((implementation architecture-mixin))
  (destructuring-bind (&key arch &allow-other-keys) (impl-mode implementation)
    (run-benchmark/arch implementation arch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions that methods in the implementation protocol may
;;; use and functions that our implementations may expect to be
;;; executed for them.

(defmethod implementation-translated-mode ((impl implementation))
  "Translates the mode of IMPL into a database-friendly string 
that can be READ back."
  (format nil "~S" (impl-mode impl)))

(defun implementation-cached-file-name (impl file-name)
  "Returns the pathname of FILE-NAME as cached for versioned implementation
IMPL."
  ;; XXX: use probe-file??
  (make-pathname :directory `(,@(pathname-directory *version-cache-dir*)
                                ,(impl-name impl)
                                ,(md5-pathname-component (implementation-translated-mode impl))
                                ,(impl-version impl)
                                ,@(when (eql :relative (first (pathname-directory file-name)))
                                    (rest (pathname-directory file-name))))
		 :name (pathname-name file-name)
		 :type (pathname-type file-name)))

(defmacro with-unzipped-implementation-files (implementation &body body)
  "Ensures that the files requires to run IMPLEMENTATION are unzipped for as long as control is in BODY.
Zips files back up again."
  (with-gensyms (impl-files impl file)
    `(let* ((,impl ,implementation)
	    (,impl-files (implementation-required-files ,impl)))
       (unwind-protect (progn
			 (loop for ,file in ,impl-files
			       do (ensure-implementation-file-unpacked ,impl ,file))
			 ,@body)
	 (loop for ,file in ,impl-files
	       do (pack-file ,impl (implementation-cached-file-name ,impl ,file)))))))

(defmacro with-current-directory (dir &body body)
  "Bind *d-p-d* to DIR and chdir there. The d-p-d binding 
and the CWD change have dynamic extent.
After control leaves BODY, chdirs back to the old value of *d-p-d*."
  ;; XXX: first bind, then chdir??
  `(unwind-protect (progn
		     #+sbcl(sb-posix:chdir ,dir)
		     (let ((*default-pathname-defaults* ,dir))
		       ,@body))
     #+sbcl(sb-posix:chdir *default-pathname-defaults*)))

(define-condition program-exited-abnormally ()
  ((program :accessor failed-program :initarg :program)
   (args :accessor failed-args :initarg :args)
   (code :accessor failed-code :initarg :code))
  (:report (lambda (c stream)
	     (format stream "Program invocation ~S ~S exited abnormally with code: ~A"
		     (failed-program c) (failed-args c) (failed-code c)))))

#+sbcl(defun invoke-logged-program (step-name program args &key (environment (sb-ext:posix-environ)))
  (multiple-value-bind (second minute hour date month year day daylight-p zone) (get-decoded-time)
      (declare (ignore day daylight-p zone))
      (let ((output-pathname (merge-pathnames
			      (make-pathname
			       :name (format nil "~4,1,0,'0@A-~2,1,0,'0@A-~2,1,0,'0@AT~2,1,0,'0@A:~2,1,0,'0@A:~2,1,0,'0@A_~A"
					     year month date hour minute second step-name))
			      *log-directory*)))
	(ensure-directories-exist output-pathname)
	(let ((proc #+sbcl(sb-ext:run-program program args
                                              :input nil
                                              :environment environment
                                              :output output-pathname
                                              :if-output-exists :supersede)
                    #-sbcl(error "Running programs is not implemented!")))
	  (sb-ext:process-wait proc)
	  (cond
            ((not (zerop (sb-ext:process-exit-code proc)))
              (error 'program-exited-abnormally
                     :program program
                     :args args
                     :code (sb-ext:process-exit-code proc)))
            (t
             (delete-file output-pathname)
             (sb-ext:process-exit-code proc)))))))

#-sbcl
(defun invoke-logged-program (step-name program args &key environment)
  (error "Failed at ~A: invoke-logged-program is unimplemented." step-name))

(defun script (name)
  (namestring (merge-pathnames (make-pathname :name name
                                              :directory '(:relative "scripts"))
                               *base-dir*)))

#+sbcl
(defmacro with-input-from-program ((stream program &rest args) &body body)
  (with-gensyms (proc arg-list)
    `(let* ((,arg-list (list ,@args))
	    (,proc (sb-ext:run-program ,program ,arg-list
				       :output :stream
				       :input nil
				       :error nil
                                       :wait nil))
	    (,stream (sb-ext:process-output ,proc)))
       ;; XXX: the order here is wrong. body shouldn't execute before
       ;; we have checked for successful program exit; but we can't
       ;; wait for that until all data has been read - which is what
       ;; the body does. argh.
       (unwind-protect (progn ,@body)
         ;; eat up the rest of the output
         (when (input-stream-p ,stream)
           (iterate (for line in-stream ,stream using #'read-line)))
	 (sb-ext:process-wait ,proc)
	 (unless (zerop (sb-ext:process-exit-code ,proc))
	   (error 'program-exited-abnormally
		  :program ,program
		  :args ,arg-list
		  :code (sb-ext:process-exit-code ,proc)))))))

#-sbcl
(defmacro with-input-from-program ((stream program &rest args) &body body)
  `(error "Can't run with input from program: Unimplemented."))

(defun implementation-already-built-p (impl)
  (every (complement #'null)
         (mapcar (lambda (file)
                   (or (probe-file (implementation-cached-zip-file-name impl file))
                       (probe-file (implementation-cached-file-name impl file))))
                 (implementation-required-files impl))))

(defun ensure-implementation-file-unpacked (impl file-name)
  (when (probe-file (implementation-cached-zip-file-name impl file-name))
    (unpack-file impl (implementation-cached-zip-file-name impl file-name)))
  file-name)

(defmethod version-from-directory :around (impl dir)
  (with-current-directory dir
    (call-next-method impl dir)))

(defmethod run-benchmark :around (impl)
  (with-current-directory *cl-bench-base*
    (call-next-method impl)))

(defmethod build-in-directory :around (impl dir)
  "After a finished build, move the required files for the
  version into the appropriate place in the build cache."
  (with-current-directory dir
    (clean-directory impl dir)
    (let ((impl (call-next-method)))
      (loop for file in (implementation-required-files impl)
	    do (rename-file (merge-pathnames (implementation-file-in-builddir impl file) dir)
			    (ensure-directories-exist
			     (implementation-cached-file-name impl file))))
      impl)))

(defun md5-pathname-component (string &key (external-format :default))
  (format nil "~(~{~2,'0X~}~)"
          (map 'list #'identity
               #+sbcl(md5sum-string string :external-format external-format)
               #-sbcl(md5sum-sequence string))))

(defun shellquote (arg quote-p)
  (if quote-p
      (format nil "'~A'" arg)
      arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; our own helper functions. Not really for public use.

(defun implementation-cached-zip-file-name (impl file-name)
  (let ((pn (implementation-cached-file-name impl file-name)))
    (make-pathname :directory (pathname-directory pn)
		   ;; XXX: suboptimal. but I can't think of a better way to fool
                   ;; the pathname parser.
		   :name (format nil "~A~@[.~A~]" (pathname-name pn) (pathname-type pn))
		   :type "gz")))

(defun unpack-file (impl file-name)
  (invoke-logged-program (format nil "unpack-~A" (impl-name impl))
                         *unpack-binary* `(,(namestring file-name))))

(defun pack-file (impl file-name)
   (invoke-logged-program (format nil "pack-~A" (impl-name impl))
                          *pack-binary* `(,(namestring file-name))))

(defmethod print-object ((o implementation) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~A" (impl-version o))))

;;; arch-tag: "251ea0cc-ff5f-11d8-8b1b-000c76244c24"
