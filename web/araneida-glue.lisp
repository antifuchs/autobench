(in-package :measure.web)

(defparameter *site-name* "beaver.boinkor.net")
(defparameter *internal-port* 8000)

(defparameter *base-url* (merge-url
			  (make-url :scheme "http"
				    :host *site-name*
				    :port *internal-port*)
			  "/bench/"))

(defparameter *webserver-url* (merge-url (make-url :scheme "http"
                                                   :host *site-name*
                                                   :port 80)
                                         "/prefab/"))

(defparameter *index-url* (merge-url *base-url* "index/"))

(defparameter *dbconn* (pg-connect "asf" "asf" :host nil))

(defparameter *prefab-base* #p"/home/asf/hack/sb-bench/+prefab/")
(defparameter *ploticus-binary* "/usr/bin/ploticus")

(defparameter *bench-listener*
  (make-instance #+sbcl #+sb-thread 'threaded-http-listener
                 #+sbcl #-sb-thread 'araneida:serve-event-http-listener
                 #-sbcl 'threaded-http-listener
                 :address #(213 235 219 107)
                 :port (araneida:url-port *base-url*)))

(defclass index-handler (handler) ())
(defclass choose-handler (handler) ())

(defun last-version ()
  (second (pg-result (pg-exec *dbconn*
                              (translate `(select (i_name version)
                                                  (limit
                                                   (order-by version
                                                             (release_date)
                                                             :desc)
                                                   :start 0 :end 1))))
                     :tuple 0)))

(defun filesys-escape-name (name)
  "Escape name so that it is safe as a file name under unixoids and DOSoids."
  (with-output-to-string (s)
    (iterate (for c in-vector name)
             (if (or (alphanumericp c) (member c '(#\- #\.)))
                 (write-char c s)
                 (progn (write-char #\_ s) (princ (char-code c) s))))))

(defun sqlquote (str)
  (with-output-to-string (s)
    (iterate (for c in-vector str)
             (case c
               (#\\
                (write-char #\\ s) (write-char #\\ s))
               (#\'
                (write-char #\' s) (write-char #\' s))
               (otherwise
                (write-char c s))))))

(defun emit-where-clause (&key benchmark implementations only-release host earliest latest)
  `(where
    (join (join (as result r)
                (as version v)
                :on (and (= r.v-name v.i-name) (= r.v-version v.version)))
          (as impl i)
          :on (= v.i-name i.name))
    (and (= r.b-name ,benchmark)
          ,(if only-release
               `(and (>= v.release-date (|::| (|::| ,earliest int4) abstime))
                     (<= v.release-date (|::| (|::| ,latest int4) abstime)))
               'is-release)
          (= m-name ,host)
          (in i.name ',implementations))))

(defun file-name-for (&key benchmark implementations only-release host &allow-other-keys)
  (make-pathname :directory `(:relative ,(filesys-escape-name (prin1-to-string implementations))
                                        ,host
                                        ,(if only-release only-release "all"))
                 :name (filesys-escape-name benchmark)))

(defun generate-image-for (&rest conditions)
  (let ((max-offset (first (pg-result (pg-exec *dbconn* (translate `(select ((max i.field_offset))
                                                                           ,(apply #'emit-where-clause conditions))))
                                               :tuple 0)))
        (filename (merge-pathnames (apply #'file-name-for conditions) *prefab-base*)))

    (with-open-file (f (ensure-directories-exist filename) :direction :output
                       :if-exists :supersede :if-does-not-exist :create)
      (iterate (for (date name version offset mean stderr) in-relation
                    (translate `(select (v.release-date v.i-name v.version i.field-offset
                                                        (avg r.seconds) (/ (stddev r.seconds) (sqrt (count r.seconds))))
                                        (order-by
                                         (group-by ,(apply #'emit-where-clause conditions)
                                                   (v.release-date v.i-name v.version i.field-offset))
                                         (v.release-date))))
                    on-connection *dbconn*)
               (format f "~A~A~f~A~f~A~%"
                       version
                       (make-string (1+ offset) :initial-element #\Tab)
                       mean
                       #\tab
                       stderr
                       (make-string (- max-offset offset) :initial-element #\Tab))))
    (measure::invoke-logged-program "gen-image" *ploticus-binary*
                                    `("-png" "-o" ,(namestring (make-pathname :type "png" :defaults filename)) "-prefab" "lines"
                                             ,(format nil "data=~A" (namestring filename)) "delim=tab" "x=1"
                                             "ygrid=yes" "xlbl=version" "ylbl=seconds" "cats=yes" "-pagesize" "15,8" "autow=yes"
                                             "yrange=0"
                                             "ynearest=5" ; used to be 0.5. now ploticus doesn't scale according to the second column. TODO?
                                             "stubvert=yes"
                                             ,@(iterate (for (impl offset) in-relation
                                                             (translate `(select (name field-offset)
                                                                                 (order-by (alias
                                                                                            (distinct
                                                                                             (select (i.name field-offset)
                                                                                                     (order-by
                                                                                                      ,(apply #'emit-where-clause conditions)
                                                                                                      (i.name))))
                                                                                            raw)
                                                                                           (field-offset))))
                                                             on-connection *dbconn*)
                                                        (for pl-s = (if (= offset 0) "" (1+ (/ offset 2))))
                                                        ;; "name=SBCL" "y=2" "err=3" "name2=CMU Common Lisp" "y2=4" "err2=5"
                                                        (collect (format nil "name~A=~A" pl-s impl))
                                                        (collect (format nil "y~A=~A" pl-s (+ 2 offset)))
                                                        (collect (format nil "err~A=~A" pl-s (+ 3 offset))))))))

(defun unix-time-to-universal-time (unix-time)
  (+ 2208988800 ; difference in seconds 1970-01-01 0:00 and 1900-01-01 0:00
     unix-time))


(defun ensure-image-files-exist (benchmarks &rest conditions)
  (let ((*latest-result* (first (pg-result (pg-exec *dbconn* "select max(date) from result") :tuple 0))))
    (declare (special *latest-result*))
    (iterate (for (benchmark) in benchmarks)
             (apply #'ensure-image-file-exists :benchmark benchmark
                    conditions))))

(defun ensure-image-file-exists (&rest conditions)
  (declare (special *latest-result*))
  (let ((filename (merge-pathnames (apply #'file-name-for conditions) *prefab-base*)))
    (unless (and (probe-file filename)
                 (probe-file (make-pathname :type "png" :defaults filename))
                 (>= (unix-time-to-universal-time (sb-posix:stat-mtime (sb-posix:stat filename)))
                     *latest-result*))
      (apply #'generate-image-for conditions))))



(defun url-for-image (&rest conditions)
  (urlstring (merge-url *webserver-url*
                          (namestring
                           (make-pathname :type "png"
                                          :defaults (apply #'file-name-for conditions))))))

(defun emit-image-index (s &rest args &key only-release implementations &allow-other-keys)
  (let* ((benchmarks (pg-result (pg-exec *dbconn* "select distinct b_name from result order by b_name") :tuples))
         (format-string "SELECT ~A(release_date)::abstime::int4 FROM VERSION AS V2 WHERE (V2.VERSION) LIKE '~A' || '.%'")
         (earliest (first (pg-result (pg-exec *dbconn* (format nil format-string "min" only-release)) :tuple 0)))
         (latest (first (pg-result (pg-exec *dbconn* (format nil format-string "max" only-release)) :tuple 0))))
    (apply #'ensure-image-files-exist benchmarks :earliest earliest :latest latest args)
    (html-stream s
      `(html
	(head (title "SBCL Benchmarks"))
	(body
         ((div :id "sidebar")
          ((form :method :post :action ,(urlstring *index-url*))
           ,(make-multi-select :implementations implementations
                               (iterate (for (impl) in-relation
                                             (translate `(select (name) impl))
                                             on-connection *dbconn*)
                                        (collect `(,impl ,impl))))
           ,(make-select :only-release only-release
                         (cons '(NIL "None")
                         (iterate (for (version steps) in-relation
                                       (let ((subsel `(select ((count *))
                                                              (where (as version v2)
                                                                     (like v2.version (|\|\|| v.version ".%"))))))
                                         (translate `(select (version ,subsel)
                                                             (where (as version v)
                                                                    (> ,subsel 0))))) 
                                       on-connection *dbconn*)
                                  (collect `(,version ,(format nil "~A (~A)" version steps))))))
           ((input :type :submit))))
         ((div :id benchmarks)
          ,@(iterate (for (benchmark) in benchmarks)
                     (collect `(h1 ,benchmark))
                     (collect `((img :src ,(apply #'url-for-image
                                                  :benchmark benchmark
                                                  :earliest earliest
                                                  :latest latest
                                                  args)))))))))))

(defun enteredp (param)
  (and param (not (equal "" param))))

(defmacro body-param-bind ((&rest args) request &body body)
  `(let (,@(loop for (arg default-value is-list) in args
                 collect `(,arg ,(if is-list
                                     `(mapcar #'cadr (body-params ,(symbol-name arg) (request-body ,request)))
                                     `(body-param ,(symbol-name arg) (request-body ,request))))))
     ,@(loop for (arg default) in args
             collect `(unless (enteredp ,arg)
                        (setf ,arg ,default)))
     ,@body))

(defmethod handle-request-response ((handler index-handler) method request)
  (request-send-headers request :expires 0)
  (let ((s (request-stream request)))
    (body-param-bind ((implementations '("SBCL" "SBCL-character") t)
                      (only-release "0.8.13.77.character")
                      (host "beaver")) request
        (emit-image-index s
                          :implementations implementations
                          :only-release only-release
                          :host host))))

(defun make-select (name default options)
  `((select :name ,name)
    ,@(loop for (op text) in options
	    if (equal default op)
	      collect `((option :value ,op :selected "") ,text)
	    else
	      collect `((option :value ,op) ,text))))

(defun make-multi-select (name selected options)
  `((select :name ,name :multiple t :size 3)
    ,@(loop for (op text) in options
	    if (member op selected :test #'string=)
	      collect `((option :value ,op :selected "") ,text)
	    else
	      collect `((option :value ,op) ,text))))

(define-condition form-value-not-entered (error)
  ((name :accessor form-value-not-entered-name :initarg :name)))

(install-handler (http-listener-handler *bench-listener*)
		 (make-instance 'index-handler)
		 (urlstring *index-url*) nil)


;; arch-tag: 05bed3a0-4ebb-4dc6-8308-3033a1c00f65
