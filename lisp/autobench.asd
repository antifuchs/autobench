;;; -*- lisp -*-

(cl:defpackage #:autobench-system
  (:use :cl :asdf))

(cl:in-package #:autobench-system)

(defsystem autobench
  :depends-on (pg sb-posix split-sequence sb-md5 common-idioms iterate net-telent-date)
  :components ((:file "package")
               (:file "variables" :depends-on ("package"))
               (:file "connection" :depends-on ("package" "variables"))
               (:file "util" :depends-on ("package" "variables"))
               (:file "implementation" :depends-on ("variables"))
               (:file "vc" :depends-on ("implementation"))
               (:file "sbcl" :depends-on ("variables" "implementation" "vc"))
               (:file "cmucl" :depends-on ("variables" "implementation"))
               (:file "clisp" :depends-on ("variables" "implementation" "vc"))
               (:file "file-locking" :depends-on ("package"))
               (:file "autobuilder"
                      :depends-on ("implementation" "util" "file-locking"))
               (:file "pg-import" :depends-on ("variables" "sbcl" "clisp"))))

(defmethod perform :after ((op load-op) (c (eql (find-system :autobench))))
  (let ((init-file (symbol-value (intern "*USER-LOCAL-INIT-FILE*" :autobench))))
    (funcall (intern "LOAD-INIT-FILE" :autobench))))

;;; arch-tag: "2522b003-ff5f-11d8-8b1b-000c76244c24"
