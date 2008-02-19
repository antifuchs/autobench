;;; -*- lisp -*-

(cl:defpackage #:autobench-web-system
  (:use :cl :asdf))

(cl:in-package #:autobench-web-system)

(defsystem autobench-web
  :depends-on (autobench pg split-sequence iterate iterate-pg :parenscript araneida alexandria sexql)
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "png-frobnicator" :depends-on ("package"))
               (:file "incremental-html" :depends-on ("package" "util"))
               (:file "araneida-glue" :depends-on ("package" "util" "incremental-html" "png-frobnicator"))
               (:file "syndication" :depends-on ("package" "araneida-glue" "util"))))



;;; arch-tag: 1c6d4937-2a40-4bda-8915-a8ce04f23b87
