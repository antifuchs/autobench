(in-package :autobench-web)

(defparameter *debugging* nil)

(defun translate* (sexql)
  (let ((sql (translate sexql)))
    (if *debugging*
        (print sql)
        sql)))

;;; arch-tag: "49c8b13b-2c1f-4b90-b6c9-24f5c381e303"
