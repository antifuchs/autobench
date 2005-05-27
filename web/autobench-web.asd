;;; -*- lisp -*-

(cl:defpackage #:autobench-web-system
  (:use :cl :asdf))

(cl:in-package #:autobench-web-system)

(defsystem autobench-web
    :depends-on (autobench pg split-sequence iterate iterate-pg araneida common-idioms sexql)
    :components ((:file "package")
                 (:file "util" :depends-on ("package"))
                 (:file "araneida-glue" :depends-on ("package" "util"))
                 (:file "syndication" :depends-on ("package" "araneida-glue" "util"))))



;;; arch-tag: 1c6d4937-2a40-4bda-8915-a8ce04f23b87