(in-package :measure)

(defparameter *machines* '("walrus.boinkor.net"))

;;; FIXME: broken
;; (defvar *conn* (pg-connect "sbclarch" "sbclarch"))



(defparameter *base-dir* #p"/home/asf/hack/sb-bench/")
(setf *default-pathname-defaults* *base-dir*)

(defparameter *plot-base* (merge-pathnames #p"plot-data/" *base-dir*))
(defparameter *cl-bench-base* (merge-pathnames #p"cl-bench/" *base-dir*))
(defparameter *log-directory* (merge-pathnames #p"+log/" *base-dir*))
(defparameter *version-cache-dir* (merge-pathnames #p"+build-archive/" *base-dir*))
(defparameter *www-base* (merge-pathnames #p"www/" *base-dir*))
(defparameter *base-result-dir* (merge-pathnames #p"to-import/*/*.*" *base-dir*))
(defparameter *version-translations-file* (merge-pathnames #p"version-translations.lisp" *base-dir*))

(defparameter *sbcl-build-args* '("lisp -batch"))

;;; arch-tag: "25252978-ff5f-11d8-8b1b-000c76244c24"
