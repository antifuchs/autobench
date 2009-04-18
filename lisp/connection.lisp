(in-package :autobench)

;;; ssh forward things

(defparameter *ssh-tunnel-host* "baker.boinkor.net")

(defparameter *ssh-remote-username* nil
  "The user name to use in order to log into autobench's db ssh forward.")

(defparameter *ssh-host* "127.0.0.1")

(defparameter *ssh-port* 5096
  "The port used as the local end point of the ssh tunnel.")

(defparameter *ssh-tunnel* nil)

#+sbcl
(progn
  (defun ensure-ssh-tunnel-connected (&optional (host "localhost") (port 5432) (remote-username *ssh-remote-username*))
    "Set up a quiet ssh tunnel between localhost and *ssh-tunnel-host*.
When using this function to connect to autobench, make sure you got an ssh user name from
the DB maintainer, installed the public key in the right place, and have customized
*ssh-remote-username* and *ssh-port* correctly."
    (cond
      ((and *ssh-tunnel* (sb-ext:process-p *ssh-tunnel*) (sb-ext:process-alive-p *ssh-tunnel*))
       *ssh-tunnel*)
      (t (setf *ssh-tunnel*
               (sb-ext:run-program "/usr/bin/ssh"
                                   `("-o" "BatchMode=yes" "-L" ,(format nil "~A:~A:~A" *ssh-port* host port) "-T"
                                          "-l" ,remote-username ,*ssh-tunnel-host* "cat")
                                   :output :stream :input :stream :error *debug-io*
                                   :wait nil))
         (format (sb-ext:process-input *ssh-tunnel*) "hello world~%")
         (finish-output (sb-ext:process-input *ssh-tunnel*))
         (read-line (sb-ext:process-output *ssh-tunnel*))
         *ssh-tunnel*)))

  (defun teardown-ssh-tunnel ()
    (when (and (sb-ext:process-p *ssh-tunnel*)
               (sb-ext:process-alive-p *ssh-tunnel*))
      (sb-ext:process-kill *ssh-tunnel* 2))
    (when (sb-ext:process-p *ssh-tunnel*)
      (sb-ext:process-wait *ssh-tunnel* t)
      (sb-ext:process-close *ssh-tunnel*))
    (setf *ssh-tunnel* nil)))

;;; the actual db connection

(defvar *default-db-connection* nil)

(defmacro with-db-connection (()  &body body)
  `(postmodern:with-connection
       (list *db-default-database-name* *db-default-user-name*
             *db-default-password* "localhost")
     ,@body))