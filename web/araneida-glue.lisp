(in-package :autobench-web)

(defparameter *site-name* "sbcl-test.boinkor.net")

(defparameter *prefab-base* #p"/home/sbcl-arch/autobench/+prefab/")
(defparameter *ploticus-binary* "/usr/bin/ploticus")

(defparameter *localhost-name* "localhost")
(defparameter *external-port* 80)

(defparameter *base-url* (merge-url
			  (make-url :scheme "http"
				    :host *site-name*
				    :port *external-port*)
			  "/bench/"))

(defparameter *internal-base-url* (let ((fwd-url (copy-url *base-url*)))
                                    (setf (url-port fwd-url) (+ 1024 (url-port *base-url*)))
                                    (setf (url-host fwd-url) *localhost-name*)
                                    fwd-url))

(defparameter *webserver-url* (merge-url (make-url :scheme "http"
                                                   :host *site-name*
                                                   :port 80)
                                         "/prefab/"))

(defparameter *index-url* *base-url*)
(defparameter *atom-url* (merge-url *base-url* "atom/")) ; see syndication.lisp

(defparameter *bench-listener* (make-instance #+sbcl #+sb-thread 'threaded-reverse-proxy-listener
                                              #+sbcl #-sb-thread 'araneida:serve-event-reverse-proxy-listener
                                              #-sbcl 'threaded-reverse-proxy-listener
                                              :address #(127 0 0 1)
                                              :port (araneida:url-port *internal-base-url*)
                                              :translations nil
                                               ;; `((,(urlstring *base-url*) ,(urlstring *internal-base-url*)))
                                              ))

(defvar *latest-result*) ; latest result entry's date in the DB

(defvar *dbconn*)

(defclass index-handler (handler) ())
(defclass atom-handler (handler) ())

(setf araneida::*handler-timeout* 240) ; XXX: make this thing faster (;

(defun last-version ()
  (second (pg-result (pg-exec *dbconn*
                              (translate* `(select (i_name version)
                                                   (limit
                                                    (order-by version
                                                              (release_date)
                                                              :desc)
                                                    :start 0 :end 1))))
                     :tuple 0)))

(defun filesys-escape-name (name)
  "Escape name so that it is safe as a file name under unixoids and DOSoids."
  (declare (string name))
  (let ((output (make-array (length name) :fill-pointer 0 :element-type 'character :adjustable t)))
    (with-output-to-string (s output)
      (iterate (for c in-vector name)
               (if (or (alphanumericp c) (member c '(#\- #\.)))
                   (write-char c s)
                   (progn (write-char #\_ s) (princ (char-code c) s)))))
    output))

(defun implementation-spec-impl (impl-spec)
  (subseq impl-spec 0 (position #\, impl-spec)))

(defun implementation-spec-mode (impl-spec)
  (subseq impl-spec (1+ (position #\, impl-spec))))

(defun emit-where-clause (&key benchmark implementations only-release host earliest latest &allow-other-keys)
  `(where
    (join (join (as result r)
                      (as build b)
                      :on (and (= r.v-name b.v-name) (= r.v-version b.v-version) (= r.mode b.mode)))
                (as version v)
                :on (and (= r.v-name v.i-name) (= r.v-version v.version)))
    (and (= r.b-name ,benchmark)
          ,(if only-release
               `(and (>= v.release-date ,earliest)
                     (<= v.release-date ,latest))
               `(or is-release
                    (>= v.release-date ,latest)))
          (= m-name ,host)
          (in b.v-name ',(mapcar #'implementation-spec-impl implementations))
          (in b.mode ',(mapcar #'implementation-spec-mode implementations)))))



(defun file-name-for (&key benchmark implementations only-release host &allow-other-keys)
  (make-pathname :directory `(:relative ,(filesys-escape-name
                                          (md5-pathname-component
                                           (prin1-to-string implementations)))
                                        ,host
                                        ,(if only-release only-release "all"))
                 :name (filesys-escape-name benchmark)))

(defun make-offset-table (&rest conditions)
  (let ((table (make-hash-table :test #'equal)))
    (values table
            (iterate (for (impl) in-relation
                  (translate*
                   `(distinct (select ((++ b.v-name "," b.mode))
                                      (order-by
                                       ,(apply #'emit-where-clause conditions)
                                       ((++ b.v-name "," b.mode)) :desc))))
                  on-connection *dbconn*)
             (for offset from 0)
             (maximizing offset)
             (setf (gethash impl table) offset)))))

(defun ploticus-offset-args (offset-table)
  (iterate (for (impl offset) in-hashtable offset-table)
           (for pl-s = (if (= offset 0) "" (+ 2 (truncate offset 2))))
           ;; "name=SBCL" "y=2" "err=3" "name2=CMU Common Lisp" "y2=4" "err2=5"
           (collect (format nil "name~A=~A" pl-s (pprint-impl-and-mode impl)))
           (collect (format nil "y~A=~A" pl-s (+ 2 (* 2 offset))))
           (collect (format nil "err~A=~A" pl-s (+ 3 (* 2 offset))))))

(defun generate-image-for (&rest conditions &key unit &allow-other-keys)
  (multiple-value-bind (offset-table max-offset) (apply #'make-offset-table conditions)
    (let ((filename (merge-pathnames (apply #'file-name-for conditions) *prefab-base*)))

      (with-open-file (f (ensure-directories-exist filename) :direction :output
                         :if-exists :supersede :if-does-not-exist :create)
        (iterate (for (date name version mean stderr) in-relation
                      (translate* `(select (v.release-date (++ b.v-name "," b.mode) v.version
                                                           (avg r.seconds) (/ (stddev r.seconds) (sqrt (count r.seconds))))
                                           (order-by
                                            (group-by ,(apply #'emit-where-clause conditions)
                                                      (v.release-date (++ b.v-name "," b.mode) v.version))
                                            (v.release-date))))
                      on-connection *dbconn*)
                 (for offset = (gethash name offset-table))
                 (format f "~A~A~f~A~f~A~%"
                         version
                         (make-string (1+ (* 2 offset)) :initial-element #\Tab)
                         mean
                         #\tab
                         stderr
                         (make-string (- max-offset offset) :initial-element #\Tab))))
      (autobench::invoke-logged-program "gen-image" *ploticus-binary*
                                        (print
                                         `("-png" "-o" ,(namestring (make-pathname :type "png" :defaults filename)) "-prefab" "lines"
                                                  ,(format nil "data=~A" (namestring filename)) "delim=tab" "x=1"
                                                  "ygrid=yes" "xlbl=version" ,(format nil "ylbl=~A" unit) "cats=yes" "-pagesize" "15,8" "autow=yes"
                                                  "yrange=0"
                                                  "ylog=log"
                                                  ;; "ynearest=0.5" ; works better with linear scale.
                                                  "stubvert=yes"
                                                  ,@(ploticus-offset-args offset-table))
                                         *debug-io*)))))

(defun unix-time-to-universal-time (unix-time)
  (declare (integer unix-time))
  (+ 2208988800 ; difference in seconds 1970-01-01 0:00 and 1900-01-01 0:00
     unix-time))

(defun ensure-image-file-exists (&rest conditions)
  (let ((filename (merge-pathnames (apply #'file-name-for conditions) *prefab-base*)))
    (unless (and (probe-file filename)
                 (probe-file (make-pathname :type "png" :defaults filename))
                 (>= (unix-time-to-universal-time (sb-posix:stat-mtime (sb-posix:stat filename)))
                     *latest-result*))
      (apply #'generate-image-for conditions))))

(defun url-for-image (&rest conditions)
  (let* ((filename (make-pathname :type "png"
                                  :defaults (apply #'file-name-for conditions)))
         (absolute-filename (merge-pathnames filename *prefab-base*)))
    (values (urlstring (merge-url *webserver-url* (namestring filename)))
          absolute-filename)))

(defun date-boundaries (&rest conditions &key host only-release &allow-other-keys)
  (declare (ignore conditions))
  (pg-result
   (pg-exec *dbconn*
            (translate* `(select ((min release-date) (max release-date))
                                 (where (join version result
                                              :on (and (= result.v-name version.i-name) (= v-version version)))
                                        (and (= m-name ,host)
                                             ,(if only-release
                                                  `(like version (++ ,only-release ".%"))
                                                  t))))))
   :tuple 0))

(defun pprint-impl-and-mode (impl-string)
  (let ((impl (implementation-spec-impl impl-string))
        (mode (implementation-spec-mode impl-string)))
    (destructuring-bind (&key arch features) (let ((*read-eval* nil))
                                               (read-from-string mode))
      (format nil "~A:~A" impl
              (string-downcase (format nil "~A~@[/~S~]" arch features))))))

(defun emit-image-index (s &rest args &key host only-release implementations &allow-other-keys)
  (let* ((benchmarks (pg-result (pg-exec *dbconn* "select distinct b_name, unit from result join benchmark on name=b_name order by b_name") :tuples))
         (date-boundaries (apply #'date-boundaries args))
         (earliest (first date-boundaries))
         (latest (second date-boundaries)))
    (iterate (for (benchmark unit) in benchmarks)
           (apply #'ensure-image-file-exists
                  :unit unit
                  :benchmark benchmark
                  :earliest earliest
                  :latest latest
                  args))
    (html-stream s
      `(html
	(head (title "SBCL Benchmarks")
              ((link :rel "stylesheet" :title "Style Sheet" :type "text/css" :href "/bench.css")))
	(body
         ((div :id "banner")
          (h1 ((a :href ,(urlstring *index-url*))
               "SBCL benchmarks"))
          (h2 "Displaying " ,(if only-release (format nil "release ~A" only-release) "all releases")))
         ((div :id "sidebar")
          ((form :method :get :action ,(urlstring *index-url*))
           ((input :type "hidden" :value ,host :name :host))
           (h2 "Machine")
           (ul
            ,@(iterate (for (machine arch) in-relation
                            (translate* `(distinct (select (m-name type) (join result machine
                                                                          :on (= result.m-name machine.name)))))
                            on-connection *dbconn*)
                       (collect `(li ((a :href ,(format nil "~A?host=~A" (urlstring *index-url*) machine)
                                         ,@(if (equal host machine)
                                               (list :class "selected")
                                               ()))
                                      ,(format nil "~A | ~A" machine arch))))))
           (h2 "Version")
           ,(make-multi-select :implementations implementations
                               (mapcar (lambda (impl) (list impl (pprint-impl-and-mode impl))) (all-implementations-of-host host)))
           (h2 "Release")
           ,(make-select :only-release only-release
                         (cons '(nil "All releases")
                               (iterate (for (version date steps) in-relation
                                             (translate*
                                              `(select (v.version v.release-date (+ 1 (count v2.version)))
                                                       (order-by
                                                        (group-by (where (join (as version v)
                                                                               (as version v2)
                                                                               :on (and (= v.i-name v2.i-name)
                                                                                        (like v2.version (++ v.version ".%"))))
                                                                         (exists (select (*)
                                                                                         (where (as result r)
                                                                                                (and (= r.m-name ,host)
                                                                                                     (= r.v-name v.i-name)
                                                                                                     (= r.v-version v2.version))))))
                                                                  (v.version v.release-date))
                                                        (v.release-date)))) 
                                             on-connection *dbconn*)
                                        (collect `(,version ,(format nil "~A (~A)" version steps))))))
           ((input :type :submit))
           (h2 "Syndicate (atom 1.0)")
           (ul
            ,@(iterate (for (machine impl) in-relation
                            (translate*
                             `(distinct
                               (select (m_name (++ v_name "," mode)) result)))
                            on-connection *dbconn*)
                       (collect `(li
                                  ((a :href ,(format nil "~A?HOST=~A&IMPLEMENTATION=~A" (urlstring *atom-url*) machine impl))
                                   ,(format nil "~A/~A" machine (pprint-impl-and-mode impl)))))))))
         ((div :id "content")
          ,@(iterate (for (benchmark unit) in benchmarks)
                     (for (values image-url filename) = (apply #'url-for-image
                                                               :benchmark benchmark
                                                               :earliest earliest
                                                               :latest latest
                                                               args))
                     (for (values width height) = (decode-width-and-height-of-png-file filename))
                     (collect `(h1 ,benchmark))
                     (collect `((a :name ,benchmark)))
                     (collect `((img :src ,image-url
                                     :width ,width
                                     :height ,height
                                     :alt ,benchmark))))))))))

(defun enteredp (param)
  (and param (not (equal "" param))))

(defmacro param-bind ((&rest args) request &body body)
  (with-gensyms (unhandled-part params argstring)
    `(let* ((,unhandled-part (request-unhandled-part ,request))
            (,argstring (subseq ,unhandled-part (mismatch ,unhandled-part "?")))
            (,params (mapcar (lambda (pp) (mapcar #'urlstring-unescape (split-sequence:split-sequence #\= pp)))
                             (split-sequence:split-sequence #\& ,argstring)))
            ,@(loop for (arg default-value is-list) in args
                    collect `(,arg ,(if is-list
                                        `(mapcar #'second
                                                 (remove-if-not (lambda (name)
                                                                  (string-equal name ,(symbol-name arg)))
                                                                ,params
                                                                :key #'first))
                                        `(second (assoc ,(symbol-name arg) ,params :test #'string-equal))))))
       ,@(loop for (arg default) in args
               collect `(cond
                          ((not (enteredp ,arg))
                           (setf ,arg ,default))
                          ((equal ,arg "NIL")
                           (setf ,arg nil))))
       ,@body)))

(defun all-implementations-of-host (host &optional preferred-only)
  (iterate (for (impl) in-relation
                (translate* `(distinct
                              (select ((++ v-name "," result.mode))
                                      (where (join result
                                                   machine-support
                                                   :on (and (= result.m-name machine-support.m-name)
                                                            (= result.v-name machine-support.i-name)
                                                            (= result.mode machine-support.mode)))
                                             (and (= ,host result.m-name)
                                                  ,@(if preferred-only
                                                        '(preferred)
                                                        nil))))))
                on-connection *dbconn*)
           (collect impl)))

(defmacro with-db-connection (connection &body body)
  `(let ((,connection (autobench:connect-to-database)))
     (unwind-protect (progn ,@body)
       (pg-disconnect ,connection))))

(defmethod handle-request-response ((handler index-handler) method request)
  (with-db-connection *dbconn*
    (let ((*latest-result* (first (pg-result (pg-exec *dbconn* "select max(date) from result") :tuple 0))))
      (request-send-headers request
                            :expires  (+ 1200 (get-universal-time))
                            :last-modified *latest-result*
                            :conditional t) 
      (let ((s (request-stream request)))
        (format s "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">")
        (param-bind ((host "baker")
                     (only-release nil)
                     (implementations (all-implementations-of-host host t) t)) request
          (emit-image-index s
                            :implementations implementations
                            :only-release only-release
                            :host host))))))

(defun make-select (name default options)
  `((select :name ,name)
    ,@(loop for (op text) in options
	    if (equal default op)
	      collect `((option :value ,op :selected "selected") ,text)
	    else
	      collect `((option :value ,op) ,text))))

(defun make-multi-select (name selected options)
  `((select :name ,name :multiple "multiple" :size 5m)
    ,@(loop for (op text) in options
	    if (member op selected :test #'string=)
	      collect `((option :value ,op :selected "selected") ,text)
	    else
	      collect `((option :value ,op) ,text))))

(araneida:attach-hierarchy (http-listener-handler *bench-listener*) *internal-base-url* *base-url*
  ("/" index-handler)
  ("/atom/"  atom-handler))


;; arch-tag: 05bed3a0-4ebb-4dc6-8308-3033a1c00f65
