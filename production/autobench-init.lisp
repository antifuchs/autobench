(setf *db-default-user-name* "autobench")
(setf *base-dir* #p"/opt/lisp/autobench/")

(defstrategy +sbcl-64-release+ sbcl (:mode (:arch :x86_64 :features ()))
             :build-p #'release-p)
(defparameter *releases-to-build* `((sbcl :directory #p"/space/lisp/autobench/+sbcl-co/sbcl/"
                                     :strategies (,+sbcl-64-release+))))