;;; -*- lisp -*-

(cl:defpackage #:measure-web-system
  (:use :cl :asdf))

(cl:in-package #:measure-web-system)

(defsystem measure-web
    :depends-on (measure pg split-sequence iterate iterate-pg araneida common-idioms sexql)
    :components ((:file "package")
                 (:file "araneida-glue" :depends-on ("package"))))



;;; arch-tag: 1c6d4937-2a40-4bda-8915-a8ce04f23b87