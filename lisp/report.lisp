;;; report.lisp
;;
;; Author: Eric Marsden  <emarsden@laas.fr>
;; rewritten by: Andreas Fuchs <asf@boinkor.net>
;; Time-stamp: <2004-03-17 22:39:34 asf>
;;
;;
;; When loaded into SBCL, this reads files from
;; *benchmark-result-base* and produces:
;;  * ploticus input files in *plot-base*
;;  * a tabular list of benchmarks' means and their standard errors


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'asdf)
  (require 'pg))

(defpackage #:reporter
  (:use #:cl #:pg))

(in-package #:reporter)

(defparameter *execute-as-standalone* t
  "output the benchmark results and exit when this file is
  loaded, if this is set to non-NIL.")

(defparameter *plot-base* (merge-pathnames #p"plot-data/"))
(defparameter *www-base* (merge-pathnames #p"www/"))

(defvar *conn* (pg-connect "sbclarch" "sbclarch"))

(defvar *latest-cvs-version* (first
			      (pg-result (pg-exec *conn*
						  "select version from version where release_date = (select max(release_date) from version);")
					 :tuple 0)))

;;; version number magic.

(defun release-p (vnum)          
  (if (search "pre" vnum)
      (<= (count #\. vnum) 1)
      (<= (count #\. vnum) 2)))

(defun release-version-number (vnum)
  "return a w.x.y version number, from a w.x.y or w.x.y.z style number."
  (declare (optimize (speed 0))
	   (type string vnum))
  (if (release-p vnum)
      vnum
      (release-version-number (subseq vnum 0 (position #\. vnum :from-end t)))))

;;; benchmark handling routines

(defun make-safe-bench-pathname (bench-name)
  (substitute #\. #\/ bench-name))

(defun open-or-retrieve (hash name directory)
  (let ((pathname (make-pathname :name (make-safe-bench-pathname name)
				 :directory directory)))
    (multiple-value-bind (val foundp) (gethash pathname hash)
      (if foundp
	  val
	  (progn
	    (ensure-directories-exist pathname)
	    (setf (gethash pathname hash)
		  (open pathname :direction :output :if-exists :append
			:if-does-not-exist :create))
	    (gethash pathname hash))))))

(defun ensure-files-open-for (hash benchmark version is-sub-version)
  (let ((new-files nil))
    (when (or (release-p version)
	      (equal *latest-cvs-version* version))
      (push (open-or-retrieve hash benchmark (pathname-directory *plot-base*)) new-files))
    (when is-sub-version
      (push (open-or-retrieve hash benchmark `(,@(pathname-directory *plot-base*) ,(release-version-number version)))
	    new-files))
    new-files))

(defun ensure-files-closed (files)
  (loop for file being the hash-value of files
	do (close file)))

(defun clean-up-dir (dir)
  "Remove old benchmark data."
  (loop for file in (directory (merge-pathnames #p"*.*" dir))
	do (ignore-errors (delete-file file))))



(defun bench-analysis.prefab ()
  (loop for dir in (directory (make-pathname :directory `(,@(pathname-directory *plot-base*) :wild)))
	do (clean-up-dir dir))
  (clean-up-dir *plot-base*)
  (let ((files (make-hash-table :test #'equal)))
    (unwind-protect
	(pg-for-each *conn* (format nil "~
                            select r1.b_name, r1.v_name, r1.v_version, avg(r1.seconds) as mean, ~
                                   stddev(r1.seconds) / sqrt(count(r1.seconds)) as stderr, ~
                                   i.field_offset, max(i2.field_offset) - i.field_offset as inv_offset, ~
                                   count(v2.version)>0 ~
                            from result r1 join version v on (r1.v_name=i_name and r1.v_version = v.version) join impl i on i_name = name ~
                                 left join version v2 on r1.v_version LIKE (v2.version || '.%') or v2.version LIKE (r1.v_version || '.%'), ~
                                 impl i2 ~
                            group by r1.b_name, r1.v_name, r1.v_version, v.release_date, i.field_offset order by r1.b_name, v.release_date;")
          (lambda (tuple)
	    (destructuring-bind (benchmark impl version mean stderr field-offset field-padding is-sub-version) tuple
	      (declare (ignore impl))
	      (dolist (file (ensure-files-open-for files benchmark version is-sub-version))
		(format file "~A~A~f~A~f~A~%"
			version
			(make-string (1+ field-offset) :initial-element #\Tab)
			mean
			#\Tab
			stderr
			(make-string field-padding :initial-element #\Tab))))))
      (ensure-files-closed files))))


(defun bench-analysis.raw ()
  "not a bench analysis per se. just output the benchmark data as a raw SEXP."
  (let ((tuples (pg-result (pg-exec *conn* (format nil "select b_name, v_name, v_version, avg(seconds) as mean, stddev(seconds) / sqrt(count(seconds)) as stderr ~
                                                        from result join version on (v_name=i_name and v_version = version) ~
                                                        group by b_name, v_name, v_version, release_date ~
                                                        order by b_name, release_date;"))
			   :tuples)))
    (with-open-file (f (merge-pathnames "raw-data.sexp"
					*www-base*)
		       :direction :output
		       :if-does-not-exist :create
		       :if-exists :supersede)
      (format f "~S" tuples))))

#|(defun output-table (release impls data)
  (multiple-value-bind (release-res benchmarks) (group-benchmark-results (gethash release data))
    (let ((impl-res nil))
      (dolist (i impls)
	(push (group-benchmark-results (gethash i data)) impl-res))
      (setf impl-res (nreverse impl-res))
      (dolist (b benchmarks)
	(format t "~&~25a" b)
	(let ((mean-ref (mean (gethash b release-res))))
	  (format t "[~10,2f|~,2f]" mean-ref (standard-error (gethash b release-res)))
	  (dolist (i impl-res)
	    (let ((impl-res-divided (mapcar (lambda (x)
					      (handler-case (/ x mean-ref) (error () -1)))
					    (gethash b i))))
	      (format t "~8,2f|~,4f" (mean impl-res-divided) (standard-error impl-res-divided)))))))))|#


#|(defun bench-analysis.text ()
  (multiple-value-bind (data implementations) (read-benchmark-data *benchmark-result-base*)
    (let ((implementation-group (make-hash-table :test #'equal))
	  (release nil)
	  (last-release nil))
      (dolist (impl implementations)
	(when (release-p (implementation-version impl))
	  (setf last-release release)
	  (setf release impl))
	(push impl (gethash (release-version-number (implementation-version impl)) implementation-group)))

      ;; for the report mail, we only want the last 2 versions + the last release
      (setf implementations (last implementations 2))
      (when (member release implementations :test #'string=)
	(setf release last-release))
      
    (format t "~32a~10@a" "Benchmark" "Reference")
    (dolist (impl implementations)
      (format t "~15@a" (implementation-version impl)))
    (format t "~%-------------------------------------------------------------------------------------~%")

    (output-table release implementations data)

    (format t "~&Reference time in first column is in seconds; other columns are relative~%")
    (format t "Reference implementation: ~a~%" release)
    (dolist (impl implementations)
      (format t "~&Impl ~a: ~a~%" (implementation-version impl) impl))
    (format t "=== Test machine ===~%")
    (format t "   Machine-instance: ~A~%" (machine-instance))
    (format t "   Machine-type: ~A~%" (machine-type))
    (format t "   Machine-version: ~A~%" (machine-version))
    #+(or cmu sbcl)
    (run-program "/usr/bin/uname" '("-a") :output t))))|#

;;; This program is meant to run as a stand-alone more-or-less script,
;;; from the build & benchmark scripts. to change that, set
;;; *execute-as-standalone* to NIL

(when *execute-as-standalone*
;;  (sb-profile:profile "REPORTER" "PG")
  (bench-analysis.prefab)
  (bench-analysis.raw)
;;  (bench-analysis.text)
;;  (sb-profile:report)
  (sb-ext:quit))

;; EOF

;;; arch-tag: "9a7b071a-ff30-11d8-8b1b-000c76244c24"
