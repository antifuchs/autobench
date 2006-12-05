;;; simple lockfile-based locking

(in-package :autobench)

(define-condition already-locked ()
  ((pathname :accessor already-locked-pathname :initarg :pathname))
  (:report (lambda (c s)
             (format s "~&Lock file pathname ~A already exists" (already-locked-pathname c)))))

(labels ((perform-lockedness-test (pathname contents)
           (let ((read-contents (make-array (1+ (length contents))
                                            :element-type '(unsigned-byte 8))))
             (with-open-file (s pathname :direction :input :element-type '(unsigned-byte 8)
                                :if-does-not-exist :error)
               (let ((posn (read-sequence read-contents s)))
                 (unless (and (= posn (length contents))
                              (equalp contents (subseq read-contents 0 posn)))
                   (error 'already-locked :pathname pathname))))))
         (create-lock-file-1 (pathname contents)
           (handler-case
               (let* ((desired-mask (logior sb-posix:s-irusr
                                            sb-posix:s-irgrp
                                            sb-posix:s-iroth))
                      (fd (sb-posix:open pathname
                                         (logior sb-posix:o-wronly sb-posix:o-creat)
                                         desired-mask))
                      (stream (sb-sys:make-fd-stream fd :output t :input nil
                                                     :file pathname
                                                     :element-type '(unsigned-byte 8))))
                 (unwind-protect (write-sequence contents stream)
                   (close stream)))
             (sb-posix:syscall-error ()
               (error 'already-locked :pathname pathname))))
         (make-lock-file-contents-string (host-name pid)
           (sb-ext:string-to-octets
            (format nil "~A-~A~%" pid host-name))))

  (defun create-lock-file (pathname &key (host-name (machine-instance)) (pid (sb-posix:getpid))
                           recursivep)
    (let ((contents (make-lock-file-contents-string host-name pid))) 
      (cond
        ((and recursivep (probe-file pathname))
         (perform-lockedness-test pathname contents)
         pathname)
        (t
         (create-lock-file-1 pathname contents)
         (perform-lockedness-test pathname contents)
         pathname))))

  (defun remove-lock-file (pathname &key (host-name (machine-instance)) (pid (sb-posix:getpid)))
    (when (probe-file pathname)
      (perform-lockedness-test pathname (make-lock-file-contents-string host-name pid))
      (delete-file pathname))))

(defmacro with-locked-pathname ((pathname &key (host-name (machine-instance)) (pid (sb-posix:getpid))
                                          recursivep)
                                &body body)
  "Executes BODY with a lock file in place for PATHNAME.

The algorithm for creating the lockfile is the same as for perl's
LockFile::Simple. Beware when using this over NFS. We do write
the host name, but perform no other form of locking on the
lockfile.

When RECURSIVEP is passed, behave as if the lockfile was just
created, if the lockfile is already held."
  (common-idioms:with-gensyms (pathname* host-name* pid* namestring)
    `(let* ((,host-name* ,host-name)
            (,pid* ,pid)
            (,namestring (namestring ,pathname))
            (,pathname* (pathname (format nil "~A.lck"
                                          (subseq ,namestring
                                                  0
                                                  (if (eql #\/
                                                           (char ,namestring
                                                                 (1- (length ,namestring))))
                                                      (1- (length ,namestring))))))))
       (unwind-protect (progn (create-lock-file ,pathname* :host-name ,host-name* :pid ,pid*
                                                :recursivep ,recursivep)
                              ,@body)
         (remove-lock-file ,pathname* :host-name ,host-name* :pid ,pid*)))))
