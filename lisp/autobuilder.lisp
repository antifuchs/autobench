(in-package :measure)

(defun implementation-already-built-p (impl)
  (with-unzipped-implementation-files impl
    (null
     (position nil
               (mapcar (lambda (file)
                         (probe-file (implementation-cached-file-name impl file)))
                       (implementation-required-files impl))))))

(defun map-over-versions-in-dir (function type directory)
  (iterate (with implementation = (make-instance type))
           (for dir initially (next-directory implementation directory) then (next-directory implementation dir))
           (while dir)
           (setf (impl-version implementation) (version-from-directory implementation dir))

           (funcall function implementation directory)))

(defun build-and-benchmark-new (type directory)
  (map-over-versions-in-dir
   (lambda (impl dir)
     (handler-case (progn
                     (unless (implementation-already-built-p impl)
                       (build-in-directory impl dir))
        	     (dotimes (i *run-benchmark-n-times*)
		       (run-benchmark impl))
                     (import-release-into-db impl (implementation-release-date impl dir))
                     (read-benchmark-data *base-result-dir* (machine-instance))
                     ;; TODO: move imported data away.
                     )
       (implementation-unbuildable ()
	 (format t "can't build ~A" impl))))
   type directory))


(defun benchmark-versions (type versions)
  (iterate (for version in versions)
           (for impl = (make-instance type :version version))
           (dotimes (i *run-benchmark-n-times*)
	     (run-benchmark impl))))

;;; arch-tag: 1c76f71a-6a6c-4423-839f-46154ea259c2
