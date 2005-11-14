(in-package :autobench-web)

(defparameter *debugging-sql* (autobench::machine-ecase
                           ("walrus.boinkor.net" nil)
                           ("baker" nil)
                           ("beaver" t)))

(defun translate* (sexql)
  (let ((sql (translate sexql)))
    (if *debugging-sql*
        (progn
          (print sexql *debug-io*)
          (print sql *debug-io*)
          (force-output *debug-io*)
          sql)
        sql)))

;;; arch-tag: "49c8b13b-2c1f-4b90-b6c9-24f5c381e303"
