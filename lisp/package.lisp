(cl:defpackage #:autobench
  (:use #:cl #:postmodern #:alexandria #:iterate
        #+sbcl #:sb-md5
        #-sbcl #:md5)
  (:export #:build-and-benchmark #:benchmark-versions #:process-benchmark-data
           #:connect-to-database #:md5-pathname-component
           #:import-release-from-dir
           ;; implementations
           #:sbcl #:sbcl-32 #:sbcl-64 #:cmucl #:cmucl-snapshot #:clisp
           #:load-init-file
           #:with-db-connection
           #:*base-dir*
           #:+benchmark-version+
           #:perform-missing-migrations))

;;; arch-tag: "2523ee5c-ff5f-11d8-8b1b-000c76244c24"
