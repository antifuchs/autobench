(in-package :autobench-web)

(defparameter *bugfix-revision* 8)

(defparameter *go-back-days* 50)
(defparameter *ignore-benchmarks* '(quote ("MAKE-LIST-SEQUENTIAL/RPLACD" "MAKE-LIST-SEQUENTIAL/PUSH-NREVERSE" "CRC40")))

(defun digits-format-string (avg stderr)
  (let ((post-dec (if (zerop stderr)
                      1
                      (ceiling (log (/ avg stderr) 10)))))
  (format nil "~~~A,~AF"
          1
          post-dec)))

(defun summarize-day/benchmark (release-p day benchmark unit impl host from to f-err t-err)
  (let ((percentage (round (* 100 (/ (abs (- to from))
                                    (max from to))))))
    `(|li| ,(format nil "<a href=\"~A\">~A</a> ~
                       ~A from (~@?~:[~2*~; &#xb1; ~@?~])~A to (~@?~:[~2*~; &#xb1; ~@?~])~A (~A~A%)" 
                    (html-escape (format nil "~A?HOST=~A&IMPLEMENTATIONS=~A~@[&ONLY-RELEASE=~A~]#~A"
                                         (urlstring *base-url*) host impl
                                         (unless release-p
                                           (release-on-day impl day))
                                         benchmark))
                    benchmark
                    (if (< from to) "<span style=\"color:#FF0000;\">increased</span>" "<span style=\"color:#00FF00;\">decreased</span>")
                    (digits-format-string from f-err) from
		    (>= f-err 1/1000)
                    (digits-format-string from f-err) f-err
                    unit 
                    (digits-format-string to t-err) to
		    (>= t-err 1/1000)
                    (digits-format-string to t-err) t-err
                    unit
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
                               (where (join version
                                            build
                                            :on (and (= version.i-name build.v-name)
                                                     (= version.version build.v-version)))
                                      (and
                                       (<= release-date
                                           (select ((max release-date))
                                                   (where version
                                                          (<= release-date
                                                              (- (to_universal_time "today") (* 86400 ,(1- day)))))))
                                       is-release
                                       (= ,(implementation-spec-impl impl) i-name)
                                       (= ,(implementation-spec-mode impl) mode)))
                               (release-date) :desc))
                      :start 0 :end 1)))
    :tuple 0)))

(defun days-with-revisions (days-back implementation)
  (iterate (for (day-nr) in-relation
                (translate*
                 `(distinct
                   (select ((as  (|::| (/ (- (to_universal_time "today") release-date) 86400) integer) day))
                           (limit
                            (order-by (where (join version
                                                   build
                                                   :on (and (= version.i-name build.v-name)
                                                            (= version.version build.v-version)))
                                             (and (= ,(implementation-spec-impl implementation) i-name)
                                                  (= ,(implementation-spec-mode implementation) mode)))
                                      (day))
                            :start 0 :end ,days-back))))
                on-connection *dbconn*
                cursor-name syndication-revision-days-cursor)
           (collect day-nr)))

(defun make-1-entry (day p-day pp-day &key implementation host &allow-other-keys)
  (iterate (for (version release-p release-date) in-relation
                (translate*
                 `(select (version is-release release-date)
                          (order-by
                           (where (join version
                                        build
                                        :on (and (= version.i-name build.v-name)
                                                 (= version.version build.v-version)))
                                             
                                  (and
                                   (= ,(implementation-spec-impl implementation) i-name)
                                   (= ,(implementation-spec-mode implementation) mode)
                                   (between release_date
                                            (- (to_universal_time "today") (* 86400 ,p-day))
                                            (- (to_universal_time "today") (* 86400 ,pp-day)))))
                           (release-date))))
                on-connection *dbconn*
                cursor-name syndication-1-entry-l1-cursor)
           (finding version maximizing release-date into last-version)
           (finding version minimizing release-date into first-version)
           (for p-version previous version
                initially (first
                           (pg-result
                            (pg-exec *dbconn*
                                     (translate*
                                      `(select (version)
                                               (order-by
                                                (where (join version
                                                             build
                                                             :on (and (= version.i-name build.v-name)
                                                                      (= version.version build.v-version)))
                                                       (and
                                                        (= ,(implementation-spec-impl implementation) i-name)
                                                        (= ,(implementation-spec-mode implementation) mode)
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
                                         (where (join version
                                                      build
                                                      :on (and (= version.i-name build.v-name)
                                                               (= version.version build.v-version)))
                                                (and
                                                 (= ,(implementation-spec-impl implementation) i-name)
                                                 (= ,(implementation-spec-mode implementation) mode)
                                                 (< release-date ,release-date)
                                                 is-release))
                                         (release-date) :desc))))
                     :tuple 0))))
           (for impl-entry =
                (iterate (for (b-name unit t-avg y-avg t-err y-err) in-relation
                              (translate*
                               ;; terminology: "y" = yesterday,
                               ;; "t" = today (which is a lie,
                               ;; because this program's yesterday
                               ;; is "the last day on which there
                               ;; was a commit")
                               `(select (t.b-name b.unit (avg t.seconds) (avg y.seconds)
                                                  (/ (stddev t.seconds) (sqrt (count t.seconds)))
                                                  (/ (stddev y.seconds) (sqrt (count y.seconds))))
                                        (having
                                         (group-by (where (join (join (as result t)
                                                                      (as benchmark b)
                                                                      :on (= t.b-name b.name))
                                                                (as result y)
                                                                :on (and (= t.v-name y.v-name) (= t.b-name y.b-name)
                                                                         (= t.m-name y.m-name) (= t.mode y.mode)))
                                                          (and
                                                           (= t.v_version ,version)
                                                           (= y.v_version ,p-version)
                                                           (not (in t.b-name ,*ignore-benchmarks*))
                                                           (= t.v_name ,(implementation-spec-impl implementation))
                                                           (= t.mode ,(implementation-spec-mode implementation))
                                                           (= t.m_name ,host)))
                                                   (t.b-name b.unit))
                                         (and
                                          (> (abs (- (avg t.seconds) (avg y.seconds)))
                                             (* 0.05 (avg y.seconds)))
                                          (> (abs (- (avg t.seconds) (avg y.seconds)))
                                             (* 3 (sqrt (+ (* (/ (stddev t.seconds) (sqrt (count t.seconds)))
                                                              (/ (stddev t.seconds) (sqrt (count t.seconds))))
                                                           (* (/ (stddev y.seconds) (sqrt (count y.seconds)))
                                                              (/ (stddev y.seconds) (sqrt (count y.seconds))))))))))))
                              on-connection *dbconn*
                              cursor-name syndication-1-entry-l2-cursor)
                         (collect (summarize-day/benchmark release-p pp-day b-name unit implementation host y-avg t-avg y-err t-err))))
           (when impl-entry
             (collect `(|li| "from " ,(pprint-impl-and-mode implementation) " revision " ,p-version " to " ,version ":"
                             (|ul| 
                              ,@impl-entry))
                      into entry-list))
           (finally (return-from make-1-entry (values first-version last-version entry-list)))))

(defun collect-entries (&rest args &key implementation host &allow-other-keys)
  ;; We need at least three (release) days' worth of benchmark data.
  (iterate (for day in (days-with-revisions *go-back-days* implementation))
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
                          |href| ,(html-escape (format nil "~A?HOST=~A&IMPLEMENTATIONS=~A&ONLY-RELEASE=~A"
                                                       (urlstring *base-url*) host implementation (release-on-day implementation day)))))
                 (|updated| ,(format-atom-decoded-time day *bugfix-revision*))
                 (|published| ,(format-atom-decoded-time day))
                 (|id| ,(html-escape
                         (format nil "tag:sbcl.boinkor.net,~A:/bench/~A/~A" (format-readable-decoded-time day) host implementation)))
                 (|author|
                  (|name| ,(format nil "~A Benchmark runner on ~A" (pprint-impl-and-mode implementation) host))
                  (|email| "sbcl-arch@boinkor.net"))
                 ((|content| |type| "xhtml" |xml:base| ,(urlstring *base-url*))
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
    `((|feed| |xmlns| "http://www.w3.org/2005/Atom"
              |xmlns:dc| "http://www.w3.org/2005/Atom"
             |xml:lang| "en")
      (|title| ,(format nil "Significant changes in ~A benchmark results on ~A" (pprint-impl-and-mode implementation) host))
      (|updated| ,(format-atom-decoded-time last-modified *bugfix-revision*))
      (|id| ,(html-escape
              (format nil "tag:sbcl.boinkor.net,~A:/bench/~A/~A" (nth-value 5 (get-decoded-time)) host implementation)))
      ((|link| |rel| "alternate"
               |type| "text/html"
               |href| ,(urlstring *base-url*)))
      ((|link| |rel| "self"
               |type| "application/atom+xml"
               |href| ,(html-escape
                        (format nil "~A?HOST=~A&IMPLEMENTATION=~A" (urlstring *atom-url*) host implementation))))
      ,@entries)))

(defmethod handle-request-response ((handler atom-handler) method request)
  (with-db-connection *dbconn*
    (let ((*latest-result* (first (pg-result (pg-exec *dbconn* "select max(date) from result") :tuple 0))))
      (request-send-headers request
                            :expires  (+ 1200 (get-universal-time)) ; TODO: set this to now+20 minutes (-:
                            :last-modified (+ *latest-result* *bugfix-revision*)
                            :conditional t
                            :content-type "text/xml; charset=utf-8"))
    (let ((s (request-stream request)))
      (format s "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
      (param-bind ((implementation "SBCL,(:ARCH :X86_64 :FEATURES NIL)")
                   (host "baker")) request
        (html-stream s
                     (emit-significant-changes :implementation implementation
                                               :host host))))))

;;; arch-tag: "17408c28-fd4d-488d-956c-17590e0c8494"
