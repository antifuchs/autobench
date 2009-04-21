(in-package :autobench-ht)

(defclass debuggable-acceptor (hunchentoot:acceptor)
     ())

(defmethod acceptor-request-dispatcher ((*acceptor* debuggable-acceptor))
  (let ((dispatcher (call-next-method)))
    (lambda (request)
      (handler-bind ((error #'invoke-debugger))
        (funcall dispatcher request)))))

(defvar *running-acceptor* nil)

(defun run-server (&key (port 4242) (debug-p t))
  (hunchentoot:start
   (or *running-acceptor*
       (setf *running-acceptor*
             (make-instance (if debug-p
                                'debuggable-acceptor
                                'acceptor)
                :port port
                :address "127.0.0.1")))))

;;; simple local handlers for the development system:

(setf *dispatch-table*
      (list 'dispatch-easy-handlers
            ;; for running in development setup:
            (create-folder-dispatcher-and-handler
             "/css/" (merge-pathnames #p"ht/public/css/" *base-dir*))
            (create-folder-dispatcher-and-handler
             "/javascript/"
             (merge-pathnames #p"ht/public/javascript/" *base-dir*))))

;;; Template default paths:

(defun find-template ()
  (let* ((uri (request-uri *request*))
         (handler-path (if (query-string *request*)
                           (subseq uri 0
                                   (1- (search (query-string *request*) uri)))
                           uri)))
    (merge-pathnames
     (merge-pathnames (pathname (concatenate 'string "ht/views" handler-path))
                      (make-pathname :type "html"))
     *base-dir*)))

(defun render-template (&rest variables)
  (with-output-to-string (output)
    (fill-and-print-template (find-template) variables :stream output)))
