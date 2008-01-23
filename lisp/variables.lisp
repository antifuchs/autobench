(in-package :autobench)

(defmacro machine-case (&rest cases)
  "Selects forms based on the value returned by (MACHINE-INSTANCE).
Usage is analogous to that of CASE, except for the lack of an otherwise clause."
  `(cond
    ,@(iterate (with had-otherwise = nil)
               (for (machine-name . forms) in cases)
               (if (eql machine-name 'otherwise)
                   (progn
                     (collect `(t ,@forms)
                       into clauses)
                     (setf had-otherwise t))
                   (collect `((equal (machine-instance) ,machine-name) ,@forms)
                     into clauses))
               (finally (if had-otherwise
                            (return clauses)
                            (return (append clauses '((t nil)))))))))

;;; activate debug messages
(defparameter *debugging* t)

(defparameter *db-connection-setup-function* (machine-case ("baker" (constantly t)))
  "A function used to prepare a DB connection.
A remote autobench machine will have this set to #'ensure-ssh-tunnel-connected,
and customize the variables documented in e-s-t-c's docstring.")

(defparameter *db-default-user-name* (machine-case
                                      ("baker" "autobench")))

;;; base directory of the sb-bench installation
(defparameter *base-dir* (machine-case
                          ("beaver" #p"/home/asf/hack/sb-bench/")
                          (otherwise (merge-pathnames #p"autobench/" (user-homedir-pathname)))))

;;; Machine-dependent stuff
(defparameter *unpack-binary* (machine-case
                                ("walrus.boinkor.net" "/usr/bin/gunzip") ; FreeBSD
                                (otherwise "/bin/gunzip")))              ; Linux
(defparameter *pack-binary* (machine-case
                                ("walrus.boinkor.net" "/usr/bin/gzip") 
                                (otherwise "/bin/gzip")))
(defparameter *tla-binary* (machine-case
                            ("walrus.boinkor.net" "/usr/local/bin/tla")
                            (otherwise #p"/usr/bin/baz")))
(defparameter *git-binary* (machine-case
                            (otherwise #p"/usr/bin/git")))
(defparameter *cogito-binary* (machine-case
                            (otherwise #p"/usr/bin/cg")))

(defparameter *tar-binary* #p"/usr/bin/tar")

(defparameter *run-benchmark-n-times* 3
  "The number of times each version should be benchmarked.
To get any useful information on variance between benchmark runs,
this should be >=3.")

(defparameter *sbcl64-build-args* '("sbcl64 --userinit /dev/null --disable-debugger")
  "64-bit native host compiler on baker.")

(defparameter *sbcl32-build-args* '("'sbcl32 --userinit /dev/null --disable-debugger'")
  "32-bit chroot host compiler.")

(defparameter *sbcl-default-build-args* '("'sbcl --userinit /dev/null --disable-debugger'")
  "Default sbcl host compiler.")


(defparameter *cmucl-snapshot-format* "../cmucl-~2,1,0,'0@A-~2,1,0,'0@A-x86-FreeBSD.tar.bz2")
;;; pathnames
(defparameter *cl-bench-base* nil)
(defparameter *log-directory* nil)
(defparameter *version-cache-dir* nil)
(defparameter *www-base* nil)
(defparameter *base-result-dir* nil)
(defparameter *archived-result-dir* nil)
(defparameter *version-translations-file* nil)

;;; autobuilding impls
(defparameter *implementations-to-build* nil)

;;; for walrus:
(defparameter *sbcl-developers* "asf@boinkor.net")

;;; pathnames

(defun set-base-pathnames ()
  (setf *cl-bench-base* (merge-pathnames #p"cl-bench/" *base-dir*))
  (setf *log-directory* (merge-pathnames #p"+log/" *base-dir*))
  (setf *version-cache-dir* (merge-pathnames #p"+build-archive/" *base-dir*))
  (setf *www-base* (merge-pathnames #p"www/" *base-dir*))
  (setf *base-result-dir* (merge-pathnames (make-pathname :directory `(:relative "+to-import" ,(machine-instance)))
                                                   *cl-bench-base*))
  (setf *archived-result-dir* (merge-pathnames (make-pathname :directory `(:relative "+to-import" "old" ,(machine-instance)))
                                                       *cl-bench-base*))
  (setf *version-translations-file* (merge-pathnames #p"version-translations.lisp-expr" *base-dir*)))

;;; local init file
(defvar *user-local-init-file* (merge-pathnames #p".autobench.lisp" (user-homedir-pathname)))

(defun load-init-file ()
  (when (probe-file *user-local-init-file*)
    (let ((*package* (find-package "AUTOBENCH")))
      (load *user-local-init-file*)))
  ;; install sanity checks 
  (check-setup)
  (set-base-pathnames))

;;; setup error checks

(defun check-setup ()
  (when (or (null *base-dir*)
            (not (probe-file *base-dir*)))
    (error "No base directory set! Please customize *base-dir* in your ~~/.autobench.lisp and run (load-init-file) again."))
  (unless *db-connection-setup-function*
    (error "No DB connection setup function set. Please customize *db-connection-setup-function* in your ~~/.autobench.lisp and run (load-init-file) again."))
  (unless *db-default-user-name*
    (error "No database user name set! Please customize *db-default-user-name* in your ~~/.autobench.lisp and run (load-init-file) again."))
  (unless *implementations-to-build*
    (warn "You have not defined any lisp implementations to autobuild. Customize *implementations-to-build*.")))

;;; arch-tag: "25252978-ff5f-11d8-8b1b-000c76244c24"
