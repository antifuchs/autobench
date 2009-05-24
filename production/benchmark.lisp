(defpackage #:autobench-benchmark
  (:use #:cl))

(in-package #:autobench-benchmark)

(handler-bind ((SB-INT:PACKAGE-AT-VARIANCE #'muffle-warning) (style-warning #'muffle-warning))
  (asdf:oos 'asdf:load-op :sb-grovel :verbose nil)
  (asdf:oos 'asdf:load-op :autobench :verbose nil)

  (funcall (intern (string-upcase "load-init-file") :autobench) #p"/opt/lisp/autobench/production/autobench-init.lisp")

  (funcall (intern (string-upcase "build-and-benchmark") :autobench))
  (sb-ext:quit))
