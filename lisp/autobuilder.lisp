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
  (lambda (revision)
   (cond
     ((release-p revision) t)
     (t (let* ((version-split (split-sequence:split-sequence #\. revision))
               (minor-number (if (> 3 (length version-split))
                                 (parse-integer (elt version-split 3))
                                 nil)))
          (and minor-number (zerop (mod minor-number n))))))))

(defstrategy +sbcl-64+ sbcl (:mode (:arch :x86_64 :features ())))
(defstrategy +sbcl-32+ sbcl (:mode (:arch :emulated-x86 :features ())))

(defstrategy +sbcl-64-threaded+ sbcl (:mode (:arch :x86_64 :features (:sb-thread)))
             :build-p (every-nth-revision 4))
(defstrategy +sbcl-32-threaded+ sbcl (:mode (:arch :emulated-x86 :features (:sb-thread)))
             :build-p (every-nth-revision 4))

(defun map-over-versions-in-dir (function base-implementation strategies additional-predicate directory)
  (iterate (with implementation = (funcall #'make-instance base-implementation))
           (for dir initially (next-directory implementation directory) then (next-directory implementation dir))
           (while dir)
           (iterate (for strategy in strategies)
                    (for s-impl = (apply 'make-instance (impl-name strategy)
                                         :version (version-from-directory implementation dir)
                                         (initargs strategy)))
                    (when (and (funcall additional-predicate (impl-version s-impl)) (funcall (build-p strategy) (impl-version s-impl))) 
                      (funcall function s-impl dir)))))

(defun build-and-benchmark-new (directory base-implementation strategies &key (additional-predicate (constantly t)))
  (map-over-versions-in-dir
   (lambda (impl dir)
     (handler-case (progn
                     (unless (implementation-already-built-p impl)
                       (format *debug-io* "~&Building ~A with mode ~S: " impl (impl-mode impl))
                       (build-in-directory impl dir)
                       (format *debug-io* " OK ")
                       (force-output *debug-io*))
                     (import-release-into-db impl (implementation-release-date impl dir) (implementation-translated-mode impl))
                     (format *debug-io* "BM~A:" *run-benchmark-n-times*)
                     (force-output *debug-io*)
                     (dotimes (i *run-benchmark-n-times*)
                       (run-benchmark impl)
                       (format *debug-io* "~A" i)
                       (force-output *debug-io*))
                     (read-benchmark-data *base-result-dir* (machine-instance)))
       (implementation-unbuildable ()
	 (format t "can't build ~A~%" impl))
       (program-exited-abnormally (c)
         (format t "Something else went wrong when autobuilding/benchmarking ~A: ~A~%" impl c))))
   base-implementation strategies additional-predicate directory))

(defun benchmark-versions (type versions &rest initargs)
  (iterate (for version in versions)
           (for impl = (apply #'make-instance type :version version initargs))
           (dotimes (i *run-benchmark-n-times*)
	     (run-benchmark impl))
           (read-benchmark-data *base-result-dir* (machine-instance))))

;;; arch-tag: 1c76f71a-6a6c-4423-839f-46154ea259c2
