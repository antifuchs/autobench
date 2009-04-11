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

;;; Incremental st-json writing:

(defvar *in-comma-separated-structure* ())
(defvar *after-first-element* ())

(defun in-comma-separated-structure-p (stream)
  (cdr (assoc stream *in-comma-separated-structure*)))

(defun after-first-element-p (stream)
  (and (in-comma-separated-structure-p stream)
       (cdr (assoc stream *after-first-element*))))

(defun (setf after-first-element-p) (new-value stream)
  (when (assoc stream *after-first-element*)
    (rplacd (assoc stream *after-first-element*) new-value)))

(defun invoke-writing-json-comma-separated-structure (stream function)
  (let ((*in-comma-separated-structure* (cons (cons stream t) *in-comma-separated-structure*))
        (*after-first-element* (cons (cons stream nil) *after-first-element*)))
    (funcall function)))

(defun invoke-writing-json-array (stream function)
  (princ #\[ stream)
  (prog1 (invoke-writing-json-comma-separated-structure stream function)
         (princ #\] stream)))

(defun invoke-writing-json-assoc (stream function)
  (princ #\{ stream)
  (prog1 (invoke-writing-json-comma-separated-structure stream function)
         (princ #\} stream)))

(defmacro writing-json-array ((stream) &body body)
  `(invoke-writing-json-array ,stream
                              (lambda () ,@body)))

(defmacro writing-json-assoc ((stream) &body body)
  `(invoke-writing-json-assoc ,stream
                             (lambda () ,@body)))

(defun write-json-assoc (stream key value)
  (st-json:write-json key stream)
  (princ #\: stream)
  (let ((*in-comma-separated-structure* nil))
    (st-json:write-json value stream)))

(defun json-assoc-key (stream key)
  (st-json:write-json key stream)
  (princ #\: stream))

(defmacro writing-assoc-value ((stream) &body body)
  `(let (*in-comma-separated-structure* (cons (cons ,stream nil) *in-comma-separated-structure*))
     ,@body))

(defmethod st-json:write-json-element :before (element stream)
  (declare (ignore element))
  (cond
    ((after-first-element-p stream)
     (princ #\, stream))
    ((in-comma-separated-structure-p stream)
     (setf (after-first-element-p stream) t))))

(defun implementation-run-times (implementation-name mode host only-release)
  (with-db-connection ()
    (let ((result-times (make-hash-table :test 'equal))
          (result-versions (make-hash-table :test 'equal)))
      (doquery (:order-by
                (:select 'benchmark-name 'release-date 'version-number
                         'version-code
                         (:as (:avg 'seconds) 'seconds)
                         (:as (:/ (:stddev 'seconds) (:count 'seconds))
                              'error)
                         :from 'results :natural :inner-join 'versions
                         :where (:and (:= 'implementation-name
                                          implementation-name)
                                      (:= 'machine-name host)
                                      (:= 'mode mode)
                                      (:raw
                                       (release-condition
                                        implementation-name
                                        only-release)))
                         :group-by 'benchmark-name 'release-date
                         'version-number 'version-code)
                (:desc 'release-date))
          (benchmark release-date version-number version-code seconds error)
        (push (list (ut-to-flot-timestamp release-date) seconds error)
              (gethash benchmark result-times ()))
        (unless (gethash (ut-to-flot-timestamp release-date)
                         result-versions)
          (setf (gethash (ut-to-flot-timestamp release-date)
                         result-versions)
                `(,version-number
                  ,@(when version-code
                      (list version-code))))))
      (st-json:write-json-to-string
       (st-json:jso "timings" result-times
                    "versions" result-versions)))))

(defun pprint-implspec (impl mode host)
  (destructuring-bind (&key arch features) (let ((*read-eval* nil))
                                             (read-from-string mode))
    (format nil "~A:~A" impl
            (string-downcase (format nil "~A~@[/~S~]~@[(on ~A)~]"
                                     arch features
                                     (when (string/= host "baker")
                                       host))))))

;;; handlers for actions:

(define-easy-handler (json-for-one-bm :uri "/bench/json/data")
    (implementation mode host release)
  (setf (hunchentoot:content-type*) "text/javascript")
  (implementation-run-times implementation mode host release))

(define-easy-handler (index :uri "/bench/index") ()
  (with-db-connection ()
    (render-template
     :implementations
     (mapcar (lambda (impl-result)
               (destructuring-bind (impl machine selected mode) impl-result
                 `(:implspec ,(format nil "~A,~A,~A" impl mode machine)
                             :pretty-name ,(pprint-implspec impl mode machine)
                             :selected ,selected)))
             (query (:order-by
                     (:select 'implementation-name 'machine-name
                              'show-by-default 'mode
                              :from 'implementations
                              :where 'show)
                     'machine-name 'implementation-name 'mode)))
     :benchmarks
     (mapcar (lambda (bm)
               `(:benchmark ,bm :id ,(gensym)))
             (query (:order-by
                     (:select 'benchmark-name :from 'benchmarks)
                     'benchmark-name)
                    :column)))))