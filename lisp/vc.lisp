;;; version-control specific protocol implementations

(in-package :autobench)

(defgeneric has-next-directory-p (impl directory)
  (:documentation "Returns whether a NEXT-DIRECTORY call would find a new directory at the time HAS-NEXT-DIRECTORY-P was called."))

(defclass git-vc-mixin () ())

(defmethod next-directory ((impl git-vc-mixin) directory)
  (let ((impl-name (class-name (class-of impl))))
    (with-current-directory directory
      (invoke-logged-program (format nil "git-fetch-~A"
                                     impl-name)
                             *git-binary* `("fetch"))
      (with-input-from-program (missing *git-binary*
                                        "rev-list" "origin" "^HEAD")
        (let* ((next-rev (iterate (for line in-stream missing using #'read-line)
                                  (for last-line previous line)
                                  (finally (return last-line)))))
          (when next-rev
            (invoke-logged-program (format nil "git-update-~A"
                                           impl-name)
                                   *git-binary* `("reset" "--hard" ,next-rev))
            directory))))))

(defmethod has-next-directory-p ((impl git-vc-mixin) directory)
  
  (let ((impl-name (class-name (class-of impl))))
    (with-current-directory directory
      (invoke-logged-program (format nil "git-fetch-~A" impl-name)
                             *git-binary* `("fetch"))
      (with-input-from-program (missing *git-binary*
                                        "rev-list" "origin" "^HEAD")
        (not (null (read-line missing nil nil)))))))

(defmethod implementation-release-date ((impl git-vc-mixin) directory)
  (with-current-directory directory
    (with-input-from-program (log *git-binary* "log" "--max-count=1")
      (let ((date-line
             (iterate (for line in-stream log using #'read-line)
                      (finding line such-that (and (= (mismatch line "Date:") 5))))))
        (net.telent.date:parse-time
         date-line
         :start 6
         :end (position #\+ date-line :from-end t))))))

