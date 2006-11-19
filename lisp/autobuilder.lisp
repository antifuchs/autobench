(in-package :autobench)

(declaim (optimize (speed 0) (space 0) (debug 2)))

(defclass strategy ()
     ((impl-name :initarg :implementation :accessor impl-name)
      (initargs :initarg :initargs :accessor initargs)
      (build-p :initarg :build-p :initform (constantly t) :accessor build-p)))

(defmacro defstrategy (name impl-name (&rest initargs) &key (build-p '(constantly t)))
  `(defparameter ,name
                 (make-instance 'strategy
                    :implementation ',impl-name
                    :initargs ',initargs
                    :build-p ,build-p)))

(defun every-nth-revision (n)
  (lambda (impl)
   (cond
     ((release-p impl) t)
     (t (let* ((revision (impl-version impl))
               (version-split (split-sequence:split-sequence #\. revision))
               (minor-number (if (> (length version-split) 3)
                                 (parse-integer (elt version-split 3))
                                 nil)))
          (and minor-number (zerop (mod minor-number n))))))))

;;; file locking
(define-condition already-locked ()
  ((pathname :accessor already-locked-pathname :initarg :pathname))
  (:report (lambda (c s)
             (format s "~&Lock file pathname ~A already exists" (already-locked-pathname c)))))

(labels ((perform-lockedness-test (pathname contents)
           (let ((read-contents (make-array (1+ (length contents))
                                            :element-type '(unsigned-byte 8))))
             (with-open-file (s pathname :direction :input :element-type '(unsigned-byte 8)
                                :if-does-not-exist :error)
               (let ((posn (read-sequence read-contents s)))
                 (unless (and (= posn (length contents))
                              (equalp contents (subseq read-contents 0 posn)))
                   (error 'already-locked :pathname pathname))))))
         (create-lock-file-1 (pathname contents)
           (handler-case
               (let* ((desired-mask (logior sb-posix:s-irusr
                                            sb-posix:s-irgrp
                                            sb-posix:s-iroth))
                      (fd (sb-posix:open pathname
                                         (logior sb-posix:o-wronly sb-posix:o-creat)
                                         desired-mask))
                      (stream (sb-sys:make-fd-stream fd :output t :input nil
                                                     :file pathname
                                                     :element-type '(unsigned-byte 8))))
                 (unwind-protect (write-sequence contents stream)
                   (close stream)))
             (sb-posix:syscall-error ()
               (error 'already-locked :pathname pathname))))
         (make-lock-file-contents-string (host-name pid)
           (sb-ext:string-to-octets
            (format nil "~A-~A~%" pid host-name))))

  (defun create-lock-file (pathname &key (host-name (machine-instance)) (pid (sb-posix:getpid))
                           recursivep)
    (let ((contents (make-lock-file-contents-string host-name pid))) 
      (cond
        ((and recursivep (probe-file pathname))
         (perform-lockedness-test pathname contents)
         pathname)
        (t
         (create-lock-file-1 pathname contents)
         (perform-lockedness-test pathname contents)
         pathname))))

  (defun remove-lock-file (pathname &key (host-name (machine-instance)) (pid (sb-posix:getpid)))
    (when (probe-file pathname)
      (perform-lockedness-test pathname (make-lock-file-contents-string host-name pid))
      (delete-file pathname))))

(defmacro with-locked-pathname ((pathname &key (host-name (machine-instance)) (pid (sb-posix:getpid))
                                          recursivep)
                                &body body)
  "Executes BODY with a lock file in place for PATHNAME.

The algorithm for creating the lockfile is the same as for perl's
LockFile::Simple. Beware when using this over NFS. We do write
the host name, but perform no other form of locking on the
lockfile.

When RECURSIVEP is passed, behave as if the lockfile was just
created, if the lockfile is already held."
  (common-idioms:with-gensyms (pathname* host-name* pid* namestring)
    `(let* ((,host-name* ,host-name)
            (,pid* ,pid)
            (,namestring (namestring ,pathname))
            (,pathname* (pathname (format nil "~A.lck"
                                          (subseq ,namestring
                                                  0
                                                  (if (eql #\/
                                                           (char ,namestring
                                                                 (1- (length ,namestring))))
                                                      (1- (length ,namestring))))))))
       (unwind-protect (progn (create-lock-file ,pathname* :host-name ,host-name* :pid ,pid*
                                                :recursivep ,recursivep)
                              ,@body)
         (remove-lock-file ,pathname* :host-name ,host-name* :pid ,pid*)))))

(defun map-over-all-versions-in-dir (function base-implementation strategies additional-predicate directory)
  "Map over all versions that follow the current version of DIRECTORY."
  (iterate (with implementation = (funcall #'make-instance base-implementation))
           (for dir initially (next-directory implementation directory) then (next-directory implementation dir))
           (while dir)
           (iterate (for strategy in strategies)
                    (for s-impl = (apply 'make-instance (impl-name strategy)
                                         :version (version-from-directory implementation dir)
                                         (initargs strategy)))
                    (when (and (funcall additional-predicate s-impl) (funcall (build-p strategy) s-impl)) 
                      (funcall function s-impl dir)))))

(defun map-over-given-versions-in-dir (function base-implementation strategies additional-predicate directory &rest version-specs)
  "Map over all versions in DIRECTORY that are in VERSION-SPECS."
  (iterate (with implementation = (funcall #'make-instance base-implementation))
           (for version-spec in version-specs)
           (for dir initially
                (directory-for-version implementation directory (first version-specs)) then
                (directory-for-version implementation dir version-spec))
           (iterate (for strategy in strategies)
                    (for s-impl = (apply 'make-instance (impl-name strategy)
                                         :version (version-from-directory implementation dir)
                                         (initargs strategy)))
                    (when (and (funcall additional-predicate s-impl) (funcall (build-p strategy) s-impl)) 
                      (funcall function s-impl dir)))))

(defun build-and-benchmark-1 (base-implementation &key directory strategies (additional-predicate (constantly t)) version-specs)
  
  (with-locked-pathname (directory)
    (apply
     (if version-specs
         #'map-over-given-versions-in-dir
         #'map-over-all-versions-in-dir)
     (lambda (impl dir)
       (handler-case (progn
                       (debug* "~&~A ~A/~S: " (get-universal-time) impl (impl-mode impl))
                       (unless (implementation-already-built-p impl)
                         (debug* "Build:")
                         (build-in-directory impl dir)
                         (debug* "OK "))
                       (import-release-into-db impl (implementation-release-date impl dir) (implementation-translated-mode impl))
                       (debug* "BM~A:" *run-benchmark-n-times*) 
                       (dotimes (i *run-benchmark-n-times*)
                         (run-benchmark impl)
                         (debug* "~A" i))
                       (read-benchmark-data *base-result-dir* (machine-instance)))
         (implementation-unbuildable ()
           (format *debug-io* "can't build ~A~%" impl))
         (program-exited-abnormally (c)
           (format *debug-io* "Something else went wrong when autobuilding/benchmarking ~A:~% ~A~%" impl c))))
     base-implementation strategies additional-predicate directory
     version-specs)))

(defun benchmark-versions (type versions &rest initargs)
  (iterate (for version in versions)
           (for impl = (apply #'make-instance type :version version initargs))
           (dotimes (i *run-benchmark-n-times*)
	     (run-benchmark impl))
           (read-benchmark-data *base-result-dir* (machine-instance))))

(defun build-and-benchmark (&key (implementations (mapcar #'first *implementations-to-build*)))
  (loop for implementation in implementations
        do (apply #'build-and-benchmark-1 (assoc implementation *implementations-to-build* :test #'eql))))

;;; arch-tag: 1c76f71a-6a6c-4423-839f-46154ea259c2
