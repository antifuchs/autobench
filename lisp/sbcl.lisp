(in-package :measure)

(defclass sbcl (implementation) ())

(defmethod build-in-directory ((implementation sbcl) directory)
  (let* ((*default-pathname-defaults* directory)
	 (*read-eval* nil)
	 (version (with-open-file (f #p"version.lisp-expr" :direction :input)
		    (read f))))
    (setf implementation (make-instance (class-of implementation)
			    :version version))
    ;; TODO: implementation building code goes here.
    implementation))

(defmethod run-benchmark ((implementation sbcl))
  ;; TODO: obviously, all functionality is missing.
  )

(defmethod implementation-required-files ((impl sbcl))
  (declare (ignore impl))
  (list #p"sbcl" #p"sbcl.core"))

(defmethod implementation-file-in-builddir ((impl sbcl) file-name)
  (declare (ignore impl))
  (cdr
   (assoc file-name
	  `((#p"sbcl" . #p"src/runtime/sbcl")
	    (#p"sbcl.core" . #p"output/sbcl.core"))
	  :test 'equal)))


;;; arch-tag: "3c5332be-00b6-11d9-a66e-000c76244c24"
