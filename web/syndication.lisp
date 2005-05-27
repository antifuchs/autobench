(in-package :autobench-web)

(defparameter *bugfix-revision* 6)

(defparameter *go-back-days* 50)
(defparameter *ignore-benchmarks* '(quote ("MAKE-LIST-SEQUENTIAL/RPLACD" "MAKE-LIST-SEQUENTIAL/PUSH-NREVERSE" "CRC40")))

(defclass atom-handler (handler) ())

(defun digits-format-string (avg stderr)
  (let ((post-dec (if (zerop stderr)
                      1
                      (ceiling (log (/ avg stderr) 10)))))
  (format nil "~~~A,~AF"
          1
          post-dec)))

(defun summarize-day/benchmark (day benchmark impl from to f-err t-err)
  (let ((percentage (round (* 100 (/ (abs (- to from))
                                    (max from to))))))
    `(|li| ,(format nil "Run time for benchmark <a href=\"~A\">~A</a> ~
                       ~A from (~@? &#xb1; ~@?)s to (~@? &#xb1; ~@?)s (~A~A%)" 
                    (html-escape (format nil "http://sbcl.boinkor.net/bench/?HOST=~A&IMPLEMENTATIONS=~A&ONLY-RELEASE=~A#~A"
                                         "walrus.boinkor.net" ; FIXME
                                         impl (release-on-day impl day) benchmark))
                    benchmark
                    (if (< from to) "<span style=\"color:#FF0000;\">increased</span>" "<span style=\"color:#00FF00;\">decreased</span>")
                    (digits-format-string from f-err) from
                    (digits-format-string from f-err) f-err
                    (digits-format-string to t-err) to
                    (digits-format-string to t-err) t-err
                    (if (< from to) "+" "-") percentage))))

(defun days-ago (days)
  (- (get-universal-time) (* 86400
                             (- days 2) ; for planet.lisp.org
                             )))

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
             (translate*
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
                (format nil "select distinct to_universal_time('today')/86400::integer - release_date/86400::integer as day ~
                             from version ~
                             where release_date between to_universal_time('today')- 86400*~A and to_universal_time('today') ~
                             order by day;"
                        days-back)
                on-connection *dbconn*)
           (collect day-nr)))

(defun make-1-entry (day p-day pp-day &key implementation host &allow-other-keys)
  (iterate (for (version release-p release-date) in-relation
                (translate*
                 `(select (version is-release release-date)
                          (order-by
                           (where version
                                  (and
                                   (= i-name ,implementation)
                                   (between release_date
                                            (- (to_universal_time "today") (* 86400 ,p-day))
                                            (- (to_universal_time "today") (* 86400 ,pp-day)))))
                           (release-date))))
                on-connection *dbconn*)
           (finding version maximizing release-date into last-version)
           (finding version minimizing release-date into first-version)
           (for p-version previous version
                initially (first
                           (pg-result
                            (pg-exec *dbconn*
                                     (translate*
                                      `(select (version)
                                               (order-by
                                                (where version
                                                       (and
                                                        (= i-name ,implementation)
                                                        (between release_date
                                                                 (- (to_universal_time "today") (* 86400 ,day))
                                                                 (- (to_universal_time "today") (* 86400 ,p-day)))))
                                                (release-date) :desc))))
                            :tuple 0)))
           (when release-p           ; compare releases to each other.
             (setf p-version
                   (first
                    (pg-result
                     (pg-exec *dbconn*
                              (translate*
                               `(select (version)
                                        (order-by
                                         (where version
                                                (and
                                                 (= i-name ,implementation)
                                                 (< release-date ,release-date)
                                                 is-release))
                                         (release-date) :desc))))
                     :tuple 0))))
           (for impl-entry =
                (iterate (for (b-name t-avg y-avg t-err y-err) in-relation
                              (translate*
                               ;; terminology: "y" = yesterday,
                               ;; "t" = today (which is a lie,
                               ;; because this program's yesterday
                               ;; is "the last day on which there
                               ;; was a commit")
                               `(select (t.b-name (avg t.seconds) (avg y.seconds)
                                                  (/ (stddev t.seconds) (sqrt (count t.seconds)))
                                                  (/ (stddev y.seconds) (sqrt (count y.seconds))))
                                        (having
                                         (group-by (where (join (as result t)
                                                                (as result y)
                                                                :on (and (= t.v-name y.v-name) (= t.b-name y.b-name)
                                                                         (= t.m-name y.m-name)))
                                                          (and
                                                           (= t.v_version ,version)
                                                           (= y.v_version ,p-version)
                                                           (not (in t.b-name ,*ignore-benchmarks*))
                                                           (= t.v_name ,implementation)
                                                           (= t.m_name ,host)))
                                                   (t.b-name))
                                         (and
                                          (> (abs (- (avg t.seconds) (avg y.seconds)))
                                             (* 0.05 (avg y.seconds)))
                                          (> (abs (- (avg t.seconds) (avg y.seconds)))
                                             (* 3 (sqrt (+ (* (/ (stddev t.seconds) (sqrt (count t.seconds)))
                                                              (/ (stddev t.seconds) (sqrt (count t.seconds))))
                                                           (* (/ (stddev y.seconds) (sqrt (count y.seconds)))
                                                              (/ (stddev y.seconds) (sqrt (count y.seconds))))))))))))
                              on-connection *dbconn*)
                         (collect (summarize-day/benchmark pp-day b-name implementation y-avg t-avg y-err t-err))))
           (when impl-entry
             (collect `(|li| "from " ,implementation " revision " ,p-version " to " ,version ":"
                             (|ul| 
                              ,@impl-entry))
               into entry-list))
           (finally (return-from make-1-entry (values first-version last-version entry-list)))))

(defun collect-entries (&rest args &key implementation host &allow-other-keys)
  (iterate (for day in (days-with-revisions *go-back-days*))
           ;; XXX: this is slightly weird. p-day the day before
           ;; yesterday or the previous day through the loop?
           ;; here, it's the previous day throught the loop.
           (for p-day previous day initially nil)
           (for pp-day previous p-day initially nil)
           (when (null pp-day)
             (next-iteration))     ; skip until we are two days ahead.
           (for (values first-version last-version entry) =
                (apply #'make-1-entry day p-day pp-day args))
           (collect
               `(|entry|
                 (|title| "Benchmark results for " ,(format-readable-decoded-time day))
                 ((|link| |rel| "alternate" |type| "text/html"
                          |href| ,(html-escape (format nil "http://sbcl.boinkor.net/bench/?HOST=~A&IMPLEMENTATIONS=~A&ONLY-RELEASE=~A"
                                                       host implementation (release-on-day implementation day)))))
                 (|modified| ,(format-atom-decoded-time day *bugfix-revision*))
                 (|issued| ,(format-atom-decoded-time day))
                 (|id| ,(format nil "tag:sbcl.boinkor.net,~A:/bench/~A" (format-readable-decoded-time day) implementation))
                 (|created| ,(format-atom-decoded-time day))
                 (|author|
                  (|name| ,(format nil "~A Benchmark runner on ~A" implementation host))
                  (|email| "sbcl-arch@boinkor.net"))
                 ((|content| |type| "application/xhtml+xml" |mode| "xml" |xml:base| "http://sbcl.boinkor.net/bench/")
                  ((|div| |xmlns| "http://www.w3.org/1999/xhtml")
                   ,(if entry
                        `(|p| "Last tested version: " ,last-version ". Significant changes:")
                        `(|p| "Benchmarked versions " ,first-version " through " ,last-version ", but there were no significant changes."))
                   (|ul|
                    ,@(when entry
                        (reverse entry)) ; get newest entries first
                    ))))
             into entries)                 
           (minimizing day into days)
           (finally (return (values entries days)))))

(defun emit-significant-changes (&rest args &key implementation host &allow-other-keys)
  (multiple-value-bind (entries last-modified) (apply #'collect-entries args)
    `((|feed| |version| "0.3"
              |xmlns| "http://purl.org/atom/ns#"
              |xmlns:dc| "http://purl.org/dc/elements/1.1/"
              |xml:lang| "en")
      (|title| ,(format nil "Significant changes in SBCL benchmark results on ~A" host))
      (|modified| ,(format-atom-decoded-time last-modified *bugfix-revision*))
      (|id| ,(format nil "tag:sbcl.boinkor.net,~A:/bench/~A" (nth-value 5 (get-decoded-time)) implementation))
      ((|link| |rel| "alternate"
               |type| "text/html"
               |href| "http://sbcl.boinkor.net/bench/"))
      ,@entries)))

(defmethod handle-request-response ((handler atom-handler) method request)
  (let ((*latest-result* (first (pg-result (pg-exec *dbconn* "select max(date) from result") :tuple 0))))
    (request-send-headers request
                          :expires  (+ 1200 (get-universal-time)) ; TODO: set this to now+20 minutes (-:
                          :last-modified (+ *latest-result* *bugfix-revision*)
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
