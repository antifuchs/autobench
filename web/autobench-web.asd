;;; -*- lisp -*-

(cl:defpackage #:autobench-web-system
  (:use :cl :asdf))

(cl:in-package #:autobench-web-system)

(defsystem autobench-web
    :depends-on (autobench pg split-sequence iterate iterate-pg araneida common-idioms sexql)
    :components ((:file "package")
                 (:file "araneida-glue" :depends-on ("package"))
                 (:file "syndication" :depends-on ("araneida-glue"))))



;;; arch-tag: 1c6d4937-2a40-4bda-8915-a8ce04f23b87