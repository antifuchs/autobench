(in-package :measure)

(defparameter *machines* '("walrus.boinkor.net"))

(defvar *conn* (pg-connect "sbclarch" "sbclarch"))



(defparameter *base-dir* #p"/home/asf/hack/sb-bench/")
(setf *default-pathname-defaults* *base-dir*)

(defparameter *plot-base* (merge-pathnames #p"plot-data/"))
(defparameter *version-cache-dir* (merge-pathnames #p"build-archive/"))
(defparameter *www-base* (merge-pathnames #p"www/"))
(defparameter *base-result-dir* (merge-pathnames #p"to-import/*/*.*"))
(defparameter *version-translations-file* (merge-pathnames #p"version-translations.lisp"))

;;; arch-tag: "25252978-ff5f-11d8-8b1b-000c76244c24"
