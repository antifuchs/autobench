(in-package :autobench-web)

(defparameter *site-name* "beaver.boinkor.net")
(defparameter *dbconn* (pg-connect "asf" "asf" :host nil))

(defparameter *prefab-base* #p"/home/asf/hack/sb-bench/+prefab/")
(defparameter *ploticus-binary* "/usr/bin/ploticus")







(defparameter *localhost-name* "localhost")
(defparameter *external-port* 80)

(defparameter *base-url* (merge-url
			  (make-url :scheme "http"
				    :host *site-name*
				    :port *external-port*)
			  "/bench/"))

(defparameter *webserver-url* (merge-url (make-url :scheme "http"
                                                   :host *site-name*
                                                   :port 80)
                                         "/prefab/"))

(defparameter *index-url* (merge-url *base-url* "index/"))

(defvar *bench-listener*)
(let ((fwd-url (copy-url *base-url*)))
  (setf (url-port fwd-url) (+ 1024 (url-port *base-url*)))
  (setf (url-host fwd-url) *localhost-name*)
  (setf *bench-listener*
        (make-instance #+sbcl #+sb-thread 'threaded-reverse-proxy-listener
                       #+sbcl #-sb-thread 'araneida:serve-event-reverse-proxy-listener
                       #-sbcl 'threaded-reverse-proxy-listener
                       :address #(127 0 0 1)
                       :port (araneida:url-port fwd-url)
                       :translations `((,(urlstring *base-url*) ,(urlstring fwd-url))))))

(defvar *latest-result*) ; latest result entry's date in the DB
(defclass index-handler (handler) ())

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
  (declare (string name))
  (let ((output (make-array (length name) :fill-pointer 0 :element-type 'character :adjustable t)))
    (with-output-to-string (s output)
      (iterate (for c in-vector name)
               (if (or (alphanumericp c) (member c '(#\- #\.)))
                   (write-char c s)
                   (progn (write-char #\_ s) (princ (char-code c) s)))))
    output))

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

(defun emit-where-clause (&key benchmark implementations only-release host earliest latest &allow-other-keys)
  `(where
    (join (join (as result r)
                (as version v)
                :on (and (= r.v-name v.i-name) (= r.v-version v.version)))
          (as impl i)
          :on (= v.i-name i.name))
    (and (= r.b-name ,benchmark)
          ,(if only-release
               `(and (>= v.release-date ,earliest)
                     (<= v.release-date ,latest))
               `(or is-release
                    (>= v.release-date ,latest)))
          (= m-name ,host)
          (in i.name ',implementations))))

(defun file-name-for (&key benchmark implementations only-release host &allow-other-keys)
  (make-pathname :directory `(:relative ,(filesys-escape-name (prin1-to-string implementations))
                                        ,host
                                        ,(if only-release only-release "all"))
                 :name (filesys-escape-name benchmark)))

(defun generate-image-for (&rest conditions &key unit &allow-other-keys)
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
    (autobench::invoke-logged-program "gen-image" *ploticus-binary*
                                      `("-png" "-o" ,(namestring (make-pathname :type "png" :defaults filename)) "-prefab" "lines"
                                               ,(format nil "data=~A" (namestring filename)) "delim=tab" "x=1"
                                               "ygrid=yes" "xlbl=version" ,(format nil "ylbl=~A" unit) "cats=yes" "-pagesize" "15,8" "autow=yes"
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
  (urlstring (merge-url *webserver-url*
                        (namestring
                         (make-pathname :type "png"
                                        :defaults (apply #'file-name-for conditions))))))

(defun date-boundaries (&rest conditions &key host only-release &allow-other-keys)
  (declare (ignore conditions))
  (pg-result
   (pg-exec *dbconn*
            (translate `(select ((min release-date) (max release-date))
                                (where (join version result
                                             :on (and (= result.v-name version.i-name) (= v-version version)))
                                       (and (= m-name ,host)
                                            ,(if only-release
                                                 `(like version (++ ,only-release ".%"))
                                                 t))))))
   :tuple 0))

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
            ,@(iterate (for (machine) in-relation
                            (translate `(distinct (select (m_name) result)))
                            on-connection *dbconn*)
                       (collect `(li ((a :href ,(format nil "~A?host=~A" (urlstring *index-url*) machine)
                                         ,@(if (equal host machine)
                                               (list :class "selected")
                                               ()))
                                      ,machine)))))
           (h2 "Version")
           ,(make-multi-select :implementations implementations
                               (iterate (for (impl) in-relation
                                             (translate `(select (name) impl))
                                             on-connection *dbconn*)
                                        (collect `(,impl ,impl))))
           (h2 "Release")
           ,(make-select :only-release only-release
                         (cons '(nil "All releases")
                               (iterate (for (version steps) in-relation
                                             (let ((subsel `(select ((count *))
                                                                    (where (as version v2)
                                                                           (like v2.version (++ v.version ".%"))))))
                                               (translate `(select (version ,subsel)
                                                                   (where (as version v)
                                                                           (> ,subsel 0)))))
                                             on-connection *dbconn*)
                                        (collect `(,version ,(format nil "~A (~A)" version steps))))))
           ((input :type :submit))))
         ((div :id "content")
          ,@(iterate (for (benchmark unit) in benchmarks)
                     (collect `(h1 ,benchmark))
                     (collect `((img :src ,(apply #'url-for-image
                                                  :benchmark benchmark
                                                  :earliest earliest
                                                  :latest latest
                                                  args)
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

(defmethod handle-request-response ((handler index-handler) method request)
  (let ((*latest-result* (first (pg-result (pg-exec *dbconn* "select max(date) from result") :tuple 0))))
    (request-send-headers request
                          :expires  (+ 1200 (get-universal-time)) ; TODO: set this to now+20 minutes (-:
                          :last-modified *latest-result*
                          :conditional t) 
    (let ((s (request-stream request)))
      (format s "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">")
      (param-bind ((implementations '("SBCL" "CMUCL") t)
                   (only-release nil) ; TODO: this mode should display the last CVS version's results.
                   (host "walrus.boinkor.net")) request
        (emit-image-index s
                          :implementations implementations
                          :only-release only-release
                          :host host)))))

(defun make-select (name default options)
  `((select :name ,name)
    ,@(loop for (op text) in options
	    if (equal default op)
	      collect `((option :value ,op :selected "selected") ,text)
	    else
	      collect `((option :value ,op) ,text))))

(defun make-multi-select (name selected options)
  `((select :name ,name :multiple "multiple" :size 3)
    ,@(loop for (op text) in options
	    if (member op selected :test #'string=)
	      collect `((option :value ,op :selected "selected") ,text)
	    else
	      collect `((option :value ,op) ,text))))

(install-handler (http-listener-handler *bench-listener*)
		 (make-instance 'index-handler)
		 (urlstring *index-url*) nil)


;; arch-tag: 05bed3a0-4ebb-4dc6-8308-3033a1c00f65
