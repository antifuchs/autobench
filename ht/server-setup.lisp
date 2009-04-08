(in-package :autobench-ht)

(defun run-server ()
  (hunchentoot:start (make-instance 'hunchentoot:acceptor :port 4242)))