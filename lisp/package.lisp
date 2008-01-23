(cl:defpackage #:autobench
  (:use #:cl #:pg #:alexandria #:iterate)
  (:export #:build-and-benchmark #:benchmark-versions #:read-benchmark-data
           #:connect-to-database #:md5-pathname-component
           #:import-release-from-dir
           ;; implementations
           #:sbcl #:sbcl-32 #:sbcl-64 #:cmucl #:cmucl-snapshot
           #:load-init-file))

;;; arch-tag: "2523ee5c-ff5f-11d8-8b1b-000c76244c24"
