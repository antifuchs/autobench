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
       (let ((manual-failed nil))
         (handler-case (progn
                         (debug* "~&~A ~A/~A: " (get-universal-time)
                                 (string-downcase (prin1-to-string impl))
                                 (string-downcase (prin1-to-string (impl-mode impl))))
                         (unless (implementation-already-built-p impl)
                           (debug* "build:")
                           (handler-bind ((manual-unbuildable
                                           (lambda (c)
                                             (declare (ignore c))
                                             (setq manual-failed t)
                                             (invoke-restart 'ignore))))
                             (build-in-directory impl dir))
                           (debug* "ok~@[/NO MANUAL~] " manual-failed))
                         (import-release-from-dir impl directory)
                         (debug* "bm~A:" *run-benchmark-n-times*) 
                         (dotimes (i *run-benchmark-n-times*)
                           (run-benchmark impl)
                           (debug* "~A" i))
                         (process-benchmark-data))
           (implementation-unbuildable (c)
             (format *debug-io* "FAIL: ~A~%" c))
           (program-exited-abnormally (c)
             (format *debug-io* "~&WEIRD ERROR during ~A:~% ~A~%" impl c)))))
     base-implementation strategies additional-predicate directory
     version-specs)))

(defun benchmark-versions (type versions &rest initargs)
  (iterate (for version in versions)
           (for impl = (apply #'make-instance type :version version initargs))
           (debug* "~&~A ~A/~A: " (get-universal-time)
                                 (string-downcase (prin1-to-string impl))
                                 (string-downcase (prin1-to-string (impl-mode impl))))
           (debug* "bm~A:" *run-benchmark-n-times*)
           (dotimes (i *run-benchmark-n-times*)
	     (run-benchmark impl)
             (debug* "~A" i))
           (process-benchmark-data)))

(defun build-and-benchmark (&key (implementations (mapcar #'first *implementations-to-build*)))
  (loop for implementation in implementations
        do (apply #'build-and-benchmark-1 (assoc implementation *implementations-to-build* :test #'eql))))

