;;; pdf-report.lisp
;;
;; Author: Eric Marsden <emarsden@laas.fr>
;; Time-stamp: <2004-01-03 emarsden>
;;
;;
;; When loaded into CMUCL, this should generate a report comparing the
;; performance of the different CL implementations which have been
;; tested. Reads the /var/tmp/CL-benchmark* files to obtain data from
;; previous runs. Requires the cl-pdf library. 

(in-package :cl-user)

(eval-when (:load-toplevel :execute)
  (require :asdf)
  (asdf:oos 'asdf:load-op :uffi)
  (asdf:oos 'asdf:load-op :cl-pdf))

(load #p"defpackage.lisp")
(load #p"support.lisp")
(load #p"tests.lisp")


(in-package :cl-bench)


(defun bench-analysis (&optional (filename #p"/tmp/cl-bench.pdf"))
  (let (data implementations benchmarks impl-scores impl-labels)
    (dolist (f (directory "/var/tmp/CL-benchmark*"))
      (ignore-errors
        (with-open-file (f f :direction :input)
          (let ((*read-eval* nil))
            (push (read f) data)))))
    (setf data (sort data #'string< :key #'car))
    (setf implementations (mapcar #'car data))
    (setf impl-scores (make-list (length implementations)
				 :initial-element 0))
    (setf impl-labels (loop :for i :from 0 :below (length implementations)
                            :collect (string (code-char (+ i (char-code #\A))))))
    (setf benchmarks (reverse (mapcar #'first (cdr (first data)))))

    ;; FIXME possibly group graphs one group per page
    ;;
    ;; add numbers on bars
    ;;
    ;; annotate each benchmark with estimated allocation volume & peak storage requirement
    (pdf:with-document (:author "Éric Marsden"
                        :title "cl-bench performance results")
      (let ((helvetica (pdf:get-font "Helvetica"))
            (helvetica-bold (pdf:get-font "Helvetica-Bold"))
            (helvetica-oblique (pdf:get-font "Helvetica-Oblique"))
            (times (pdf:get-font "Times-Roman"))
            (page 0)
            (page-name ""))
        (dolist (bn benchmarks)
          (let ((results (loop :for i :in implementations
                               :collect (let* ((id (cdr (assoc i data :test #'string=)))
                                               (ir (third (assoc bn id :test #'string=))))
                                          (if (numberp ir) (float ir) -0.02))))
                (b (find bn *benchmarks* :test #'string= :key #'benchmark-name)))
            (incf page)
            (setf page-name (format nil "page ~d" page))
            (pdf:with-page ()
              (pdf:register-page-reference page-name)
              (pdf:with-outline-level (bn page-name)
                ;; version number
                (pdf:in-text-mode
                 (pdf:set-font helvetica-oblique 8)
                 (pdf:move-text 10 10)
                 (pdf:draw-text (format nil "cl-bench version ~A" *version*)))
                ;; test title
                (pdf:in-text-mode
                 (pdf:set-font helvetica-bold 16.0)
                 (pdf:move-text 100 540)
                 (pdf:draw-text bn))
                ;; optional extra description
                (when (and b (benchmark-long b))
                  (pdf:in-text-mode
                   (pdf:set-font helvetica 12)
                   (pdf:move-text 100 520)
                   (pdf:draw-text (benchmark-long b))))
                ;; y-axis title
                (pdf:with-saved-state
                    (pdf:translate 65 350)
                  (pdf:rotate 90)
                  (pdf:in-text-mode
                   (pdf:set-font helvetica 10)
                   (pdf:draw-text "seconds")))
                (pdf:draw-object
                 (make-instance 'pdf:histogram
                                :x 100 :y 300 :width 200 :height 200
                                :label-names impl-labels
                                :labels&colors '(("ignore" (0.0 0.0 1.0)))
                                :series (list results)
                                :y-axis-options '(:min-value 0 :title "seconds")
                                :background-color '(0.9 0.9 0.9)))
                (pdf:in-text-mode
                 (pdf:move-text 100 250)
                 (pdf:set-font times 12.0)
                 (dotimes (i (length implementations))
                   (pdf:move-text 0 -14)
                   (pdf:draw-text (format nil "~A: ~A"
                                          (nth i impl-labels)
                                          (nth i implementations))))))))))
      (pdf:write-document filename))))


(bench-analysis)
(quit)

;; EOF
