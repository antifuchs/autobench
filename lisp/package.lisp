(cl:defpackage #:autobench
  (:use #:cl #:pg #:common-idioms #:iterate)
  (:export #:build-and-benchmark-new #:benchmark-versions #:read-benchmark-data
           ;; implementations
           #:sbcl #:cmucl #:cmucl-snapshot))

;;; arch-tag: "2523ee5c-ff5f-11d8-8b1b-000c76244c24"
