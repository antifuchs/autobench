(in-package :autobench-ht)

;;; handlers for actions:

(define-easy-handler (index-redirect :uri "/boinkmarks/") ()
  (redirect "/boinkmarks/index" :code +http-moved-permanently+))

(define-easy-handler (index :uri "/boinkmarks/index") ()
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
                     (:select 'benchmark-name :from 'benchmarks
                              :where (:= 'benchmark-version
                                         +benchmark-version+))
                     'benchmark-name)
                    :column)))))

(define-easy-handler (json-for-one-bm :uri "/boinkmarks/json/data")
    (implementation mode host release)
  (setf (hunchentoot:content-type*) "text/javascript")
  (with-db-connection ()
    (let ((last-result
           (query (:select
                   (:max 'result-date)
                   :from 'versions :natural :inner-join 'results
                   :where (:raw (result-condition
                                 implementation mode host release)))
                  :single)))
      (when last-result
        (handle-if-modified-since last-result))
      (setf (header-out :last-modified) (rfc-1123-date last-result)
            (header-out :expires) (rfc-1123-date (+ (* 20 60)
                                                    (get-universal-time))))))
  (implementation-run-times implementation mode host release))

;;; functions used by these handlers:

(defun implementation-run-times (implementation-name mode host only-release)
  (with-db-connection ()
    (let ((result-times (make-hash-table :test 'equal))
          (result-versions (make-hash-table :test 'equal)))
      ;; timings:
      (doquery (:order-by
                (:select 'benchmark-name 'release-date
                         (:as (:avg 'seconds) 'seconds)
                         (:as (:/ (:stddev 'seconds) (:count 'seconds))
                              'error)
                         :from 'results :natural :inner-join 'versions
                         :where (:raw (result-condition implementation-name
                                                        mode host only-release))
                         :group-by 'benchmark-name 'release-date
                         'version-number 'version-code)
                (:desc 'release-date))
          (benchmark release-date seconds error)
        (push (list (ut-to-flot-timestamp release-date) seconds error)
              (gethash benchmark result-times ())))
      ;; version info:
      (doquery (:select 'release-date 'version-number 'version-code
                        (:as (:raw
                              (subversion-exists-condition
                               implementation-name mode host))
                             'zoomp)
                        :from 'versions
                        :where (:and (:= 'implementation-name
                                         implementation-name)
                                     (:raw
                                      (release-condition
                                       implementation-name
                                       only-release))))
          (release-date version-number version-code zoomp)
        (setf (gethash (ut-to-flot-timestamp release-date)
                       result-versions)
              `(,version-number ,(st-json:as-json-bool zoomp)
                                ,@(when version-code
                                    (list version-code)))))
      (st-json:write-json-to-string
       (st-json:jso "timings" result-times
                    "versions" result-versions)))))

(defun result-condition (implementation-name mode host only-release)
  (sql (:and (:= 'implementation-name
                 implementation-name)
             (:= 'machine-name host)
             (:= 'benchmark-version
                 +benchmark-version+)
             (:= 'mode mode)
             (:raw
              (release-condition implementation-name only-release)))))

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

(defun subversion-exists-condition (implementation-name mode host)
  (sql (:exists
        (:select 'version-id :from (:as 'versions 'subversions)
                 :where (:and (:= 'subversions.implementation-name
                                  implementation-name)
                              (:= 'subversions.belongs-to-release
                                  'versions.version-number)
                              (:not
                               (:= 'subversions.belongs-to-release
                                   'subversions.version-number))
                              (:exists
                               (:select 'version-id
                                        :from (:as 'results 'sv-results)
                                        :where (:and
                                                (:= 'sv-results.version-id
                                                    'subversions.version-id)
                                                (:= 'sv-results.machine-name
                                                    host)
                                                (:= 'sv-results.mode mode)
                                                (:= 'benchmark-version
                                                    +benchmark-version+)))))))))

(defun pprint-implspec (impl mode host)
  (destructuring-bind (&key arch features) (let ((*read-eval* nil))
                                             (read-from-string mode))
    (format nil "~A:~A" impl
            (string-downcase (format nil "~A~@[/~S~]~@[(on ~A)~]"
                                     arch features
                                     (when (string/= host "baker")
                                       host))))))

(defun ut-to-flot-timestamp (timestamp)
  (* (- timestamp 2208988800) 1000))
