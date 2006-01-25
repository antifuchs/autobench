(in-package :autobench)

(defun debug* (format-string &rest args)
  (when *debugging*
    (apply #'format *debug-io* format-string args)
    (force-output *debug-io*)))

;;; arch-tag: "d60a022b-89fc-45aa-8540-82fb88ff1c08"
