(in-package :autobench)

;;; ssh forward things

(defparameter *ssh-tunnel-host* "baker.boinkor.net")

(defparameter *ssh-remote-username* nil
  "The user name to use in order to log into autobench's db ssh forward.")

(defparameter *ssh-host* "127.0.0.1")

(defparameter *ssh-port* 5096
  "The port used as the local end point of the ssh tunnel.")

(defparameter *ssh-tunnel* nil)

(defun ensure-ssh-tunnel-connected (&optional (host "localhost") (port 5432) (remote-username *ssh-remote-username*))
  "Set up a quiet ssh tunnel between localhost and *ssh-tunnel-host*.
When using this function to connect to autobench, make sure you got an ssh user name from
the DB maintainer, installed the public key in the right place, and have customized
*ssh-remote-username* and *ssh-port* correctly."
  (if (and *ssh-tunnel* (sb-ext:process-p *ssh-tunnel*) (sb-ext:process-alive-p *ssh-tunnel*))
      *ssh-tunnel*
      (setf *ssh-tunnel*
            (sb-ext:run-program "/usr/bin/ssh"
                                `("-o" "BatchMode=yes" "-CNL" ,(format nil "~A:~A:~A" *ssh-port* host port) "-T"
                                       "-u" ,remote-username ,*ssh-tunnel-host*)
                                :output nil :input nil :error nil
                                :wait nil))))

(defun teardown-ssh-tunnel ()
  (when (and (sb-ext:process-p *ssh-tunnel*)
	     (sb-ext:process-alive-p *ssh-tunnel*))
    (sb-ext:process-kill *ssh-tunnel* 2))
  (when (sb-ext:process-p *ssh-tunnel*)
    (sb-ext:process-wait *ssh-tunnel* t)
    (sb-ext:process-close *ssh-tunnel*))
  (setf *ssh-tunnel* nil))

;;; the actual db connection

(defvar *default-db-connection* nil)

(defun db-connection ()
  "Return the default database connection; if none was made yet, connect and return the new connection."
  (when (null *default-db-connection*)
    (setf *default-db-connection* (connect-to-database)))
  *default-db-connection*)

(defun connect-to-database ()
  (when (funcall *db-connection-setup-function*)
    (machine-case
     ("walrus.boinkor.net" (pg-connect "sbcl-arch" "sbcl-arch"))
     ("beaver" (pg-connect "asf" "asf"))
     ("baker" (pg-connect "sbcl-arch" "sbcl-arch" :host #p"/tmp/"))
     (otherwise (pg-connect "sbcl-arch" *db-default-user-name* :host "localhost" :port *ssh-port*)))))

;;; arch-tag: "197175bc-1afe-48e5-97a4-904d500c1988"
