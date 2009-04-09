(cl:defpackage :autobench-ht-system
  (:use :cl :asdf))

(cl:in-package :autobench-ht-system)

(defsystem autobench-ht
    :depends-on (:autobench :hunchentoot :st-json :html-template)
    :components ((:file "package")
                 (:file "server-setup" :depends-on ("package"))))