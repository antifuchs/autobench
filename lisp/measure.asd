;;; -*- lisp -*-

(cl:defpackage #:measure-system
  (:use :cl :asdf))

(cl:in-package #:measure-system)

(defsystem measure
    :depends-on (pg sb-posix split-sequence common-idioms iterate net-telent-date)
    :components ((:file "package")
		 (:file "variables" :depends-on ("package"))
		 (:file "txt-sql" :depends-on ("variables"))
		 (:file "implementation" :depends-on ("variables"))
		 (:file "sbcl" :depends-on ("variables" "implementation"))
		 (:file "autobuilder" :depends-on ("implementation"))))

;;; arch-tag: "2522b003-ff5f-11d8-8b1b-000c76244c24"
