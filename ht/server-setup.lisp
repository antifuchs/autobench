(in-package :autobench-ht)

(defclass debuggable-acceptor (hunchentoot:acceptor)
     ())

(defmethod acceptor-request-dispatcher ((*acceptor* debuggable-acceptor))
  (let ((dispatcher (call-next-method)))
    (lambda (request)
      (handler-bind ((error #'invoke-debugger))
        (funcall dispatcher request)))))

(defvar *running-acceptor* nil)

(defun run-server (&key (debug-p t))
  (hunchentoot:start
   (or *running-acceptor*
       (setf *running-acceptor*
             (make-instance (if debug-p
                                'debuggable-acceptor
                                'acceptor)
                :port 4242)))))

;;; simple local handlers for the development system:

(setf *dispatch-table*
      (list 'dispatch-easy-handlers
            ;; for running in development setup:
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
