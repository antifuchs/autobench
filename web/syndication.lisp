(in-package :autobench-web)

(defparameter *go-back-days* 50)
(defparameter *stddev-dev* 0.1)
(defparameter *ignore-benchmarks* '(quote ("MAKE-LIST-SEQUENTIAL/RPLACD" "MAKE-LIST-SEQUENTIAL/PUSH-NREVERSE" "CRC40")))

(defclass atom-handler (handler) ())

(defun explain-change (day benchmark impl from-revision to-revision from to)
  `(|li| ,(format nil "Run time for benchmark <a href=\"~A\">~A</a> ~
                       ~A from ~As to ~As between ~A revision ~A and ~A"
                   (html-escape (format nil "http://sbcl.boinkor.net/bench/?HOST=~A&IMPLEMENTATIONS=~A&ONLY-RELEASE=~A#~A"
                                        "walrus.boinkor.net" ; FIXME
                                        impl (release-on-day impl day) benchmark))
                    benchmark
                   (if (< from to) "increased" "decreased")
                   from to impl from-revision to-revision)))

(defun revision-on-day (day p-day impl benchmark which)
  (apply #'values
         (pg-result
          (pg-exec *dbconn*
                   (translate
                    `(select (version (avg seconds))
                             (group-by
                              (where (join version result
                                           :on (and (= v-name i-name) (= v-version version)))
                                     (and (= i-name ,impl)
                                          (= b-name ,benchmark)
                                          (= release-date
                                             (select ((,which release-date))
                                                     (where
                                                      version
                                                      (and
                                                       (= i-name ,impl)
                                                       (between release-date
                                                                (- (to_universal_time "today") (* 86400 ,day))
                                                                (- (to_universal_time "today") (* 86400 ,p-day)))))))))
                              (version)))))
          :tuple 0)))

(defun summarize-day/benchmark (day p-day pp-day b-name v-name)
  (multiple-value-bind (first-r first-sec) (revision-on-day day p-day v-name b-name 'min)
    (multiple-value-bind (last-r last-sec) (revision-on-day p-day pp-day v-name b-name 'max)
      (explain-change day b-name v-name first-r last-r first-sec last-sec))))

(defun days-ago (days)
  (- (get-universal-time) (* 86400 days)))

(defun format-atom-decoded-time (&optional (days 0) (minute-as-version 0))
  (multiple-value-bind (second minute hour date month year day daylight-p zone) (decode-universal-time (days-ago days))
    (declare (ignore day hour minute second daylight-p zone))
    (format nil "~4,1,0,'0@A-~2,1,0,'0@A-~2,1,0,'0@AT~2,1,0,'0@A:~2,1,0,'0@A:~2,1,0,'0@AZ"
            year month date 0 minute-as-version 0)))

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

(defun days-with-revisions (days-back)
  (iterate (for (day-nr) in-relation
                (format nil "select distinct to_universal_time('today')/86400::integer - release_date/86400::integer ~
                             from version ~
                             where release_date between to_universal_time('today')- 86400*~A and to_universal_time('today');"
                        days-back)
                on-connection *dbconn*)
           (collect day-nr)))

(defun emit-significant-changes (&rest args &key implementation host &allow-other-keys)
  (declare (ignore args))
  (multiple-value-bind (entries last-modified)
      (iterate (for day in (days-with-revisions *go-back-days*))
               (for p-day previous day initially nil)
               (for pp-day previous p-day initially nil)
               (when (null pp-day)
                 (next-iteration)) ; skip until we are two days ahead.
               (for entry =
                    (iterate (for (v-name b-name) in-relation
                                  (translate
                                   ;; terminology: "y" = yesterday,
                                   ;; "t" = today (which is a lie,
                                   ;; because this program's yesterday
                                   ;; is "the last day on which there
                                   ;; was a commit")
                                   `(select (t.v-name t.b-name)
                                            (having
                                             (group-by (where (join (join (as result t) (as version t-v)
                                                                          :on (and (= t.v-name t-v.i-name) (= t.v-version t-v.version)))
                                                                    (join (as result y) (as version y-v)
                                                                          :on (and (= y.v-name y-v.i-name) (= y.v-version y-v.version)))
                                                                    :on (and (= t.v-name y.v-name) (= t.b-name y.b-name) (= t.m-name y.m-name)))
                                                              (and
                                                               (between t-v.release_date
                                                                        (- (to_universal_time "today") (* 86400 ,day))
                                                                        (- (to_universal_time "today") (* 86400 ,p-day)))
                                                               (between y-v.release-date
                                                                        (- (to_universal_time "today") (* 86400 ,p-day))
                                                                        (- (to_universal_time "today") (* 86400 ,pp-day)))
                                                               (not (in t.b-name ,*ignore-benchmarks*))
                                                               (= t.v_name ,implementation)
                                                               (= t.m_name ,host)))
                                                       (t.v-name t.b-name))
                                             (and
                                              (> (abs (- (avg t.seconds) (avg y.seconds)))
                                                 (* 0.05 (avg y.seconds)))
                                              (> (abs (- (avg t.seconds) (avg y.seconds)))
                                                 (* 3 (sqrt (+ (* (/ (stddev t.seconds) (sqrt (count t.seconds)))
                                                                  (/ (stddev t.seconds) (sqrt (count t.seconds))))
                                                               (* (/ (stddev y.seconds) (sqrt (count y.seconds)))
                                                                  (/ (stddev y.seconds) (sqrt (count y.seconds))))))))))))
                                  on-connection *dbconn*)
                             (collect (summarize-day/benchmark day p-day pp-day b-name v-name))))
               (when entry
                 (collect
                     `(|entry|
                       (|title| "Benchmark results for " ,(format-readable-decoded-time day))
                       ((|link| |rel| "alternate" |type| "text/html"
                                |href| ,(html-escape (format nil "http://sbcl.boinkor.net/bench/?HOST=~A&IMPLEMENTATIONS=~A&ONLY-RELEASE=~A"
                                                host implementation (release-on-day implementation day)))))
                       (|modified| ,(format-atom-decoded-time day 1))
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
    (|modified| ,(format-atom-decoded-time last-modified 1))
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
