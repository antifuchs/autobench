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

(defun implementation-run-times (stream
                                 implementation-name mode host benchmark
                                 only-release)
  (with-db-connection ()
    (let ((result-times ())
          (result-versions ())
          (result-errors ()))
      (doquery (:order-by
                (:select 'release-date 'version-number
                         (:as (:avg 'seconds) 'seconds)
                         (:as (:/ (:stddev 'seconds) (:count 'seconds)) 'error)
                         :from 'results :natural-inner-join 'versions
                         :where (:and (:= 'implementation-name
                                          implementation-name)
                                      (:= 'machine-name host)
                                      (:= 'mode mode)
                                      (:= 'benchmark-name benchmark)
                                      (:raw
                                       (release-condition implementation-name
                                                          only-release)))
                         :group-by 'release-date 'version-number)
                'release-date)
               (release-date version-number seconds error)
               (push (list (ut-to-flot-timestamp release-date)
                           seconds)
                     result-times)
               (push (list (ut-to-flot-timestamp release-date)
                           (+ seconds error) (- seconds error))
                     result-errors)
               (push (cons (ut-to-flot-timestamp release-date)
                           `#(,version-number ,implementation-name))
                     result-versions))
      (st-json:write-json (list (nreverse result-times)
                                (nreverse result-errors)
                                #+nil(nreverse result-versions))
                          stream))))

;;; handlers for actions:

(define-easy-handler (json-for-one-bm :uri "/bench/json/data")
    (implementation mode host benchmark release)
  (setf (hunchentoot:content-type*) "text/javascript")
  (with-output-to-string (out)
   (implementation-run-times out implementation mode host benchmark release)))

(hunchentoot:define-easy-handler (static-json :uri "/bench/json/dummy")
    ()
  "[[[1114370936000, 3.016666666666667e-1], [1117131100000, 3.12e-1], [1119903045000, 3.1266666666666665e-1], [1122330766000, 3.1266666666666665e-1], [1125075618000, 3.12e-1], [1127825895000, 3.116666666666667e-1], [1130373796000, 3.113333333333333e-1], [1133196772000, 3.113333333333333e-1], [1135700334000, 3.08e-1], [1138300151000, 3.12e-1], [1141006083000, 3.1066666666666665e-1], [1143413954000, 3.013333333333333e-1], [1146065363000, 3.013333333333333e-1], [1148759200000, 3.036666666666667e-1], [1151343632000, 3.0e-1], [1153943420000, 3.0e-1], [1156537602000, 3.0266666666666664e-1], [1159288015000, 2.9866666666666664e-1], [1161784720000, 2.9866666666666664e-1], [1164850603000, 3.0e-1], [1167141464000, 3.1066666666666665e-1], [1169736694000, 3.053333333333333e-1], [1172603022000, 3.16e-1], [1174837194000, 3.053333333333333e-1], [1177782478000, 3.1866666666666665e-1], [1180224808000, 3.16e-1], [1182984249000, 3.133333333333333e-1], [1185367173000, 3.093333333333333e-1], [1188169815000, 3.093333333333333e-1], [1190733504000, 3.053333333333333e-1], [1193344083000, 3.1066666666666665e-1], [1196033635000, 3.1466666666666665e-1], [1198757549000, 3.0266666666666664e-1], [1201469587000, 3.133333333333333e-1], [1204033707000, 3.08e-1], [1208047141000, 3.12e-1], [1211972404000, 3.1066666666666665e-1], [1214766685000, 3.133333333333333e-1], [1220231141000, 3.12e-1], [1222951242000, 2.9466666666666663e-1], [1225400326000, 3.013333333333333e-1], [1228140273000, 3.0e-1], [1230726441000, 3.0266666666666664e-1], [1233625752000, 2.92e-1]], [[1114370936000, 3.0217584174388123e-1, 3.0115749158945215e-1], [1117131100000, 3.1266666666666854e-1, 3.1133333333333146e-1], [1119903045000, 3.1336055533315643e-1, 3.1197277800017686e-1], [1122330766000, 3.130515668461293e-1, 3.12281766487204e-1], [1125075618000, 3.123333333333381e-1, 3.116666666666619e-1], [1127825895000, 3.1236055533315504e-1, 3.1097277800017836e-1], [1130373796000, 3.1171823351279326e-1, 3.109484331538733e-1], [1133196772000, 3.121722038261422e-1, 3.104944628405244e-1], [1135700334000, 3.09333333333334e-1, 3.06666666666666e-1], [1138300151000, 3.13333333333334e-1, 3.10666666666666e-1], [1141006083000, 3.114364670255865e-1, 3.0989686630774677e-1], [1143413954000, 3.021031336922545e-1, 3.005635329744121e-1], [1146065363000, 3.018425084105519e-1, 3.008241582561147e-1], [1148759200000, 3.0468501682110083e-1, 3.0264831651223256e-1], [1151343632000, 3.0133333333333323e-1, 2.9866666666666675e-1], [1153943420000, 3.023094010767592e-1, 2.976905989232408e-1], [1156537602000, 3.034364670255865e-1, 3.0189686630774676e-1], [1159288015000, 2.994364670255865e-1, 2.9789686630774675e-1], [1161784720000, 3.0174586810234516e-1, 2.955874652309881e-1], [1164850603000, 3.0133333333333323e-1, 2.9866666666666675e-1], [1167141464000, 3.114364670255865e-1, 3.0989686630774677e-1], [1169736694000, 3.061031336922545e-1, 3.045635329744121e-1], [1172603022000, 3.1952766841475283e-1, 3.124723315852472e-1], [1174837194000, 3.068729340511724e-1, 3.037937326154942e-1], [1177782478000, 3.194364670255852e-1, 3.178968663077481e-1], [1180224808000, 3.1952766841475283e-1, 3.124723315852472e-1], [1182984249000, 3.1537003364220256e-1, 3.1129663302446403e-1], [1185367173000, 3.101031336922545e-1, 3.085635329744121e-1], [1188169815000, 3.108729340511724e-1, 3.077937326154942e-1], [1190733504000, 3.0737003364220306e-1, 3.032966330244635e-1], [1193344083000, 3.114364670255865e-1, 3.0989686630774677e-1], [1196033635000, 3.1543646702558653e-1, 3.1389686630774677e-1], [1198757549000, 3.047033669755359e-1, 3.006299663577974e-1], [1201469587000, 3.1537003364220256e-1, 3.1129663302446403e-1], [1204033707000, 3.080000020277912e-1, 3.079999979722088e-1], [1208047141000, 3.120000014338649e-1, 3.119999985661351e-1], [1211972404000, 3.127033669755364e-1, 3.086299663577969e-1], [1214766685000, 3.1487293405117306e-1, 3.1179373261549354e-1], [1220231141000, 3.120000014338649e-1, 3.119999985661351e-1], [1222951242000, 2.967033669755364e-1, 2.9262996635779687e-1], [1225400326000, 3.021031336922545e-1, 3.005635329744121e-1], [1228140273000, 3.01333333333334e-1, 2.9866666666666597e-1], [1230726441000, 3.042062673845064e-1, 3.011270659488269e-1], [1233625752000, 2.943094010767592e-1, 2.896905989232408e-1]]]")

(define-easy-handler (index :uri "/bench/index") ()
  (with-db-connection ()
    (render-template :benchmarks
                     (mapcar (lambda (bm)
                               `(:benchmark ,bm))
                             (query (:order-by
                                     (:select 'benchmark-name :from 'benchmarks)
                                     'benchmark-name)
                                    :column)))))