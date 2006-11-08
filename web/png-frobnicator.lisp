;;; a few simple functions to find out the width & height of a PNG
;;; image, fast.

(in-package :autobench-web)

(defun swap-endianness (byte)
  (let ((new-word 0))
    (setf (ldb (byte 8 0) new-word) (ldb (byte 8 24) byte)
          (ldb (byte 8 8) new-word) (ldb (byte 8 16) byte)
          (ldb (byte 8 16) new-word) (ldb (byte 8 8) byte)
          (ldb (byte 8 24) new-word) (ldb (byte 8 0) byte))
    new-word))

(defun decode-width-and-height-of-png-stream (stream)
  "Read IHDR data from a seekable (unsigned-byte 32) stream."
  (let ((old-position (file-position stream)))
    (unwind-protect
        (progn
          (file-position stream 4)
          (let ((width (cl:read-byte stream))
                (height (cl:read-byte stream)))
            (values
             (swap-endianness width)
             (swap-endianness height))))
      (file-position stream old-position))))

(defun decode-width-and-height-of-png-file (pathname)  
  (when (probe-file filename)
    (with-open-file (s pathname :element-type '(unsigned-byte 32))
      (decode-width-and-height-of-png-stream s))))

;;; arch-tag: "e6f076cb-91b2-4c41-a0c0-a7ecd67c55d4"
