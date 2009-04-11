(in-package :autobench-ht)

(defclass debuggable-acceptor (hunchentoot:acceptor)
     ())

(defmethod acceptor-request-dispatcher ((*acceptor* debuggable-acceptor))
  (let ((dispatcher (call-next-method)))
    (lambda (request)
      (handler-bind ((error #'invoke-debugger))
        (funcall dispatcher request)))))

(defvar *running-acceptor* nil)

(defun run-server ()
  (hunchentoot:start
   (or *running-acceptor*
       (setf *running-acceptor*
             (make-instance 'debuggable-acceptor :port 4242)))))

;;; simple local handlers for the development system:

(setf *dispatch-table*
      (list 'dispatch-easy-handlers
            (create-folder-dispatcher-and-handler
             "/css/" (merge-pathnames #p"public/css/" *base-dir*))
            (create-folder-dispatcher-and-handler
             "/js/" (merge-pathnames #p"public/js/" *base-dir*))))

;;; Template default paths:

(defun find-template ()
  (let* ((uri (request-uri *request*))
         (handler-path (if (query-string *request*)
                           (subseq uri 0
                                   (1- (search (query-string *request*) uri)))
                           uri)))
    (merge-pathnames
     (merge-pathnames (pathname (concatenate 'string "views" handler-path))
                      (make-pathname :type "html"))
     *base-dir*)))

(defun render-template (&rest variables)
  (with-output-to-string (output)
    (fill-and-print-template (find-template) variables :stream output)))

;;; need to move these:

(defun ut-to-flot-timestamp (timestamp)
  (* (- timestamp 2208988800) 1000))

(defun release-condition (implementation-name only-release)
  (if (null only-release)
      (sql
       (:or 'is-release
            (:= 'versions.version-id
                (query (:select
                        'version-id :from 'versions
                        :where (:and (:= 'implementation-name
                                         implementation-name)
                                     (:= 'release-date
                                         (:select
                                          (:max 'release-date)
                                          :from 'versions
                                          :where (:= 'implementation-name
                                                     implementation-name)))))
                       :single))))
      (sql (:and (:= 'implementation-name implementation-name)
                 (:= 'belongs-to-release only-release)))))

(defun implementation-run-times (implementation-name mode host
                                 only-release)
  (with-db-connection ()
    (let ((result-times (make-hash-table :test 'equal))
          (result-versions (make-hash-table :test 'equal))
          (result-errors (make-hash-table :test 'equal)))
      (doquery (:order-by
                (:select 'benchmark-name 'release-date 'version-number
                         (:as (:avg 'seconds) 'seconds)
                         (:as (:/ (:stddev 'seconds) (:count 'seconds)) 'error)
                         :from 'results :natural :inner-join 'versions
                         :where (:and (:= 'implementation-name
                                          implementation-name)
                                      (:= 'machine-name host)
                                      (:= 'mode mode)
                                      (:raw
                                       (release-condition implementation-name
                                                          only-release)))
                         :group-by 'benchmark-name 'release-date
                         'version-number)
                (:desc 'release-date))
               (benchmark release-date version-number seconds error)
               (push (list (ut-to-flot-timestamp release-date) seconds)
                     (gethash benchmark result-times ()))
               (push (list (ut-to-flot-timestamp release-date) error)
                     (gethash benchmark result-errors ()))
               (unless (gethash (ut-to-flot-timestamp release-date)
                                result-versions)
                 (setf (gethash (ut-to-flot-timestamp release-date)
                                result-versions)
                       version-number)))
      (st-json:write-json-to-string (list result-times
                                          result-errors
                                          result-versions)))))

;;; handlers for actions:

(define-easy-handler (json-for-one-bm :uri "/bench/json/data")
    (implementation mode host release)
  (setf (hunchentoot:content-type*) "text/javascript")
  (implementation-run-times implementation mode host release))

(define-easy-handler (index :uri "/bench/index") ()
  (with-db-connection ()
    (render-template :benchmarks
                     (mapcar (lambda (bm)
                               `(:benchmark ,bm :id ,(gensym)))
                             (query (:order-by
                                     (:select 'benchmark-name :from 'benchmarks)
                                     'benchmark-name)
                                    :column)))))