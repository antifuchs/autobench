(defpackage #:autobench-init
  (:use #:cl))

(in-package #:autobench-init)

(defvar *swank-port* 7717)
(defvar *ht-port* 7718)

(asdf:oos 'asdf:load-op :autobench-ht)
(asdf:oos 'asdf:load-op :swank)

(autobench::load-init-file #p"/opt/lisp/autobench/production/autobench-init.lisp")

(swank:create-server :port *swank-port* :dont-close t)
(autobench-ht:run-server :port *ht-port* :debug-p nil)