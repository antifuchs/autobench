(in-package :autobench)

(defmacro machine-ecase (&rest cases)
  "Selects forms based on the value returned by (MACHINE-INSTANCE).
Usage is analogous to that of ECASE."
  `(cond
    ,@(iterate (for (machine-name . forms) in cases)
               (collect `((equal (machine-instance) ,machine-name) ,@forms)))
    (t (error "Don't know which branch to use for machine name ~S" (machine-instance)))))


;;; Connection to the SB-BENCH database
(defvar *dbconn* (machine-ecase
                   ("walrus.boinkor.net" (pg-connect "sbcl-arch" "sbcl-arch"))
                   ("beaver" (pg-connect "asf" "asf" :host nil))))

;;; base directory of the sb-bench installation
(defparameter *base-dir* (machine-ecase
                          ("walrus.boinkor.net" #p"/home/sbcl-arch/autobench/")
                          ("beaver" #p"/home/asf/hack/sb-bench/")))

;;; Machine-dependent stuff
(defparameter *unpack-binary* (machine-ecase
                                ("walrus.boinkor.net" "/usr/bin/gunzip") ; FreeBSD
                                ("beaver" "/bin/gunzip")))               ; Linux
(defparameter *pack-binary* (machine-ecase
                                ("walrus.boinkor.net" "/usr/bin/gzip") 
                                ("beaver" "/bin/gzip")))
(defparameter *tla-binary* (machine-ecase
                            ("walrus.boinkor.net" "/usr/local/bin/tla")
                            ("beaver" #p"/usr/bin/tla")))

(defparameter *tar-binary* #p"/usr/bin/tar")

(defparameter *run-benchmark-n-times* 3
  "The number of times each version should be benchmarked.
To get any useful information on variance between benchmark runs,
this should be >=3.")

(defparameter *sbcl-build-args* '("lisp -batch")
  "The program that the SBCL build should use as a host compiler.
For improved compilation speed, I recommend using CMUCL 18e (-:")


(defparameter *cmucl-snapshot-format* "../cmucl-~2,1,0,'0@A-~2,1,0,'0@A-x86-FreeBSD.tar.bz2")
;;; pathnames
(defparameter *cl-bench-base* (merge-pathnames #p"cl-bench/" *base-dir*))
(defparameter *log-directory* (merge-pathnames #p"+log/" *base-dir*))
(defparameter *version-cache-dir* (merge-pathnames #p"+build-archive/" *base-dir*))
(defparameter *www-base* (merge-pathnames #p"www/" *base-dir*))
(defparameter *base-result-dir* (merge-pathnames (make-pathname :directory `(:relative "+to-import" ,(machine-instance)))
                                                 *cl-bench-base*))
(defparameter *archived-result-dir* (merge-pathnames (make-pathname :directory `(:relative "+to-import" "old" ,(machine-instance)))
                                                     *cl-bench-base*))
(defparameter *version-translations-file* (merge-pathnames #p"version-translations.lisp-expr" *base-dir*))

;;; for walrus:
(defparameter *sbcl-developers* "asf@boinkor.net")

;;; arch-tag: "25252978-ff5f-11d8-8b1b-000c76244c24"
