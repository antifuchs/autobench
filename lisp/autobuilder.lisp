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
  (iterate (with implementation = (funcall #'make-instance base-implementation))
           (for dir initially (next-directory implementation directory) then (next-directory implementation dir))
           (while dir)
           (iterate (for strategy in strategies)
                    (for s-impl = (apply 'make-instance (impl-name strategy)
                                         :version (version-from-directory implementation dir)
                                         (initargs strategy)))
                    (when (and (funcall additional-predicate (impl-version s-impl)) (funcall (build-p strategy) s-impl)) 
                      (funcall function s-impl dir)))))

(defun map-over-given-versions-in-dir (function base-implementation strategies additional-predicate directory &rest version-specs)
  (iterate (with implementation = (funcall #'make-instance base-implementation))
           (for version-spec in version-specs)
           (for dir initially
                (directory-for-version implementation directory version-spec) then
                (directory-for-version implementation dir version-spec))
           (iterate (for strategy in strategies)
                    (for s-impl = (apply 'make-instance (impl-name strategy)
                                         :version (version-from-directory implementation dir)
                                         (initargs strategy)))
                    (when (and (funcall additional-predicate (impl-version s-impl)) (funcall (build-p strategy) s-impl)) 
                      (funcall function s-impl dir)))))

(defun build-and-benchmark-1 (base-implementation &key directory strategies (additional-predicate (constantly t)) version-specs)
  (apply
   (if version-specs
       #'map-over-given-versions-in-dir
       #'map-over-all-versions-in-dir)
   (lambda (impl dir)
     (handler-case (progn
                     (debug* "~&~A/~S: " impl (impl-mode impl))
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
         (format *debug-io* "Something else went wrong when autobuilding/benchmarking ~A: ~A~%" impl c))))
   base-implementation strategies additional-predicate directory
   version-specs))

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
