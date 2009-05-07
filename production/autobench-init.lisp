(setf *db-default-user-name* "autobench")
(setf *base-dir* #p"/opt/lisp/autobench/")


(defstrategy +sbcl-32/rebuild+ sbcl (:mode (:arch :emulated-x86 :features ())) :build-p #'release-p)
(defstrategy +sbcl-32-threaded/rebuild+ sbcl (:mode (:arch :emulated-x86 :features (:sb-thread)))
             :build-p #'release-p)

(defparameter *32impls-to-rebuild* `((sbcl :directory #p "/home/sbcl-arch/space/autobench/+newest-64/"
                                      :strategies (,+sbcl-32/rebuild+ ,+sbcl-32-threaded/rebuild+))))