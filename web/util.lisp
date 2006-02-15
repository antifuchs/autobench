(in-package :autobench-web)

(defparameter *debugging-sql* (autobench::machine-case
                           ("walrus.boinkor.net" nil)
                           ("baker" nil)
                           ("beaver" t)))

(defun translate-optimization (opt)
  (let ((opt-name (if (consp opt) (second opt) opt))
        (opt-value (if (and (consp opt)
                            (string-equal (first opt) :NOT))
                       nil
                       t)))
    (values
     (string-downcase (substitute #\_ #\- (format nil "~A" opt-name)))
     opt-value)))

(defun translate-optimization-value (value)
  (if value
      "on"
      "off"))

#|
;;; XXX: doesn't work, unfortunately.
 (defun augment-with-sql-optimizations (optimizations sql)
  (let ((augmented-sql
         (labels ((translate-sql-optimization (optimization set-p)
                    (multiple-value-bind (name value) (translate-optimization optimization)
                      (format nil "set ~A to ~A;" name (translate-optimization-value
                                                        (if set-p
                                                            value
                                                            (not value)))))))
           (with-output-to-string (s)
             (dolist (enable optimizations)
               (format s "~A" (translate-sql-optimization enable t)))
             (format s "~A" sql)
             (format s ";")
             (dolist (disable optimizations)
               (format s "~A" (translate-sql-optimization disable nil)))))))
    (if *debugging-sql*
        (print augmented-sql *debug-io*)
        augmented-sql)))
|#

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
