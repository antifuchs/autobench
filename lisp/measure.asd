;;; -*- lisp -*-

(defpackage #:measure-system
  (:use :cl :asdf))

(in-package #:measure-system)

(defsystem measure
    :depends-on (pg sb-posix split-sequence common-idioms)
    :components ((:file "package")
		 (:file "variables" :depends-on ("package"))
		 (:file "txt-sql" :depends-on ("variables"))
		 (:file "report" :depends-on ("variables"))
		 (:file "implementation" :depends-on ("variables"))
		 ;;(:file "build" :depends-on ("implementation" "report" "txt-sql"))
		 ))

;;; arch-tag: "2522b003-ff5f-11d8-8b1b-000c76244c24"
