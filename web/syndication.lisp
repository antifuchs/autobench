(in-package :autobench-web)

(defparameter *go-back-days* 50)
(defparameter *stddev-dev* 2)
(defparameter *ignore-benchmarks* '(quote ("CORE-FILE-SIZE" "MAKE-LIST-SEQUENTIAL/RPLACD" "MAKE-LIST-SEQUENTIAL/PUSH-NREVERSE" "CRC40")))

(defclass atom-handler (handler) ())

(defun explain-change (day benchmark impl from-revision to-revision from to)
  `(|li| ,(format nil "runtime for benchmark <a href=\"http://sbcl.boinkor.net/bench/?HOST=~A&IMPLEMENTATIONS=~A&ONLY-RELEASE=~A#~A\">~A</a> ~
                       ~A from ~As to ~As between ~A revision ~A and ~A"
                  "walrus.boinkor.net" ; FIXME
                  impl (release-on-day impl day) benchmark
                  benchmark
                  (if (< from to) "increased" "decreased")
                  from to impl from-revision to-revision)))

(defun summarize-day/benchmark (day b-name v-name results)
  (let ((first-r (first results))
        (last-r (first (last results))))
    (destructuring-bind (from-version from-sec _) first-r
      (declare (ignore _))
      (destructuring-bind (to-version to-sec _) last-r
        (declare (ignore _))
        (when (> (abs (- to-sec from-sec)) *stddev-dev*)
          (explain-change day b-name v-name from-version to-version from-sec to-sec))))))

(defun days-ago (days)
  (- (get-universal-time) (* 86400 days)))

(defun format-atom-decoded-time (&optional (days 0))
  (multiple-value-bind (second minute hour date month year day daylight-p zone) (decode-universal-time (days-ago days))
    (declare (ignore day hour minute second daylight-p zone))
    (format nil "~4,1,0,'0@A-~2,1,0,'0@A-~2,1,0,'0@AT~2,1,0,'0@A:~2,1,0,'0@A:~2,1,0,'0@AZ"
            year month date 0 0 0)))

(defun format-readable-decoded-time (&optional (days 0))
  (multiple-value-bind (second minute hour date month year day daylight-p zone) (decode-universal-time (days-ago days))
    (declare (ignore day hour minute second daylight-p zone))
    (format nil "~4,1,0,'0@A-~2,1,0,'0@A-~2,1,0,'0@A"
            year month date)))

(defun release-on-day (impl day)
  (first
   (pg-result
    (pg-exec *dbconn*
              (translate
              `(limit (select (version)
                              (order-by
                               (where version
                                      (and
                                       (<= release-date
                                          (select ((max release-date))
                                                  (where version
                                                         (<= release-date
                                                             (- (to_universal_time "today") (* 86400 ,(1- day)))))))
                                       is-release
                                       (= ,impl i-name)))
                               (release-date) :desc))
                      :start 0 :end 1)))
    :tuple 0)))

(defun emit-significant-changes (&rest args &key implementation host &allow-other-keys)
  (declare (ignore args))
  (multiple-value-bind (entries last-modified)
      (iterate (for day from 0 to *go-back-days*)
               (for entry =
                    (iterate (for (v-name b-name stddev) in-relation
                                   (translate
                                   `(select (v-name b-name (stddev seconds))
                                            (having
                                             (group-by (where result
                                                              (and
                                                               (between date
                                                                        (- (to_universal_time "today") (* 86400 ,(1+ day)))
                                                                        (- (to_universal_time "today") (* 86400 ,day)))
                                                               (not (in b-name ,*ignore-benchmarks*))
                                                               (= v_name ,implementation)
                                                               (= m_name ,host)))
                                                       (v-name b-name))
                                             (> (stddev seconds) ,*stddev-dev*))))
                                  on-connection *dbconn*)
                             (for day-summary =
                                  (summarize-day/benchmark
                                   day b-name v-name
                                   (pg-result
                                    (pg-exec *dbconn*
                                              (translate
                                              `(select (v-version (avg seconds) release-date)
                                                       (order-by (group-by (where (join result version
                                                                                        :on (and (= v-name i-name) (= v-version version)))
                                                                                  (and
                                                                                   (between date
                                                                                            (- (to_universal_time "today") (* 86400 ,(1+ day)))
                                                                                            (- (to_universal_time "today") (* 86400 ,day)))
                                                                                   (= b-name ,b-name)
                                                                                   (= v_name ,v-name)
                                                                                   (= m_name ,host)))
                                                                           (v-version release-date))
                                                                 (release-date)))))
                                    :tuples)))
                             (when day-summary
                               (collect day-summary))))
               (when entry
                 (collect
                     `(|entry|
                       (|title| "Benchmark results for " ,(format-readable-decoded-time day))
                       ((|link| |rel| "alternate" |type| "text/html"
                                |href| ,(html-escape (format nil "http://sbcl.boinkor.net/bench/?HOST=~A&IMPLEMENTATIONS=~A&ONLY-RELEASE=~A"
                                                host implementation (release-on-day implementation day)))))
                       (|modified| ,(format-atom-decoded-time day))
                       (|issued| ,(format-atom-decoded-time day))
                       (|id| ,(format nil "tag:sbcl.boinkor.net,~A:/bench/~A" (format-readable-decoded-time day) implementation))
                       (|created| ,(format-atom-decoded-time day))
                       (|author|
                        (|name| ,(format nil "SBCL Benchmark runner on ~A" host))
                        (|email| "sbcl-arch@boinkor.net"))
                       ((|content| |type| "application/xhtml+xml" |mode| "xml" |xml:base| "http://sbcl.boinkor.net/bench/")
                        ((|div| |xmlns| "http://www.w3.org/1999/xhtml")
                         (|ul|
                          ,@entry))))
                   into entries)
                 (minimizing day into days))
               (finally (return (values entries days))))
  `((|feed| |version| "0.3"
            |xmlns| "http://purl.org/atom/ns#"
            |xmlns:dc| "http://purl.org/dc/elements/1.1/"
            |xml:lang| "en")
    (|title| ,(format nil "Significant changes in SBCL benchmark results on ~A" host))
    (|modified| ,(format-atom-decoded-time last-modified))
    (|id| ,(format nil "tag:sbcl.boinkor.net,~A:/bench/~A" (nth-value 5 (get-decoded-time)) implementation))
    ((|link| |rel| "alternate"
             |type| "text/html"
             |href| "http://sbcl.boinkor.net/bench/"))
    ,@entries)))

(defmethod handle-request-response ((handler atom-handler) method request)
  (let ((*latest-result* (first (pg-result (pg-exec *dbconn* "select max(date) from result") :tuple 0))))
    (request-send-headers request
                          :expires  (+ 1200 (get-universal-time)) ; TODO: set this to now+20 minutes (-:
                          :last-modified *latest-result*
                          :conditional t
                          :content-type "text/xml; charset=utf-8"))
  (let ((s (request-stream request)))
    (format s "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
    (param-bind ((implementation "SBCL" t)
                 (host "walrus.boinkor.net")) request
      (html-stream s
                   (emit-significant-changes :implementation implementation
                                             :host host)))))

(install-handler (http-listener-handler *bench-listener*)
		 (make-instance 'atom-handler)
		 (urlstring *atom-url*) nil)

;;; arch-tag: "17408c28-fd4d-488d-956c-17590e0c8494"
