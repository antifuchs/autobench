(in-package :autobench-web)

(defvar *default-tag-stream*)

(defun invoke-tag (tagoid stream continuation)
  (macrolet ((continue-with-output ((format-before &rest args-before) (format-after &rest args-after))
               `(progn
                  (format stream ,format-before ,@args-before)
                  (force-output stream)
                  (funcall continuation)
                  (format stream ,format-after ,@args-after)
                  (force-output stream))))
    (let ((things (list tagoid nil)))
      (cond ((htmlp things)
             (destructure-html (tag attrs content) things
               (declare (ignore content))
               (when (get tag :html-converter)
                 (error "Tag ~A has :html-converter set, so will not work with incremental html display." tag))
               (case (intern (symbol-name tag) :keyword)
                 (:comment
                  (continue-with-output ("<!-- ") (" -->")))
                 #+parenscript
                 (:css
                  (continue-with-output ("<style type=\"text/css\">~%<!--~%")
                                        ("~%--></style>")))
                 #+parenscript
                 (:js-script
                  (continue-with-output ("<script type=\"text/javascript\">~%// <![CDATA[~%")
                                        ("~%// ]]>~%</script>~%")))
                 (otherwise
                  (continue-with-output ("<~A~A>" (string-downcase (symbol-name tag))
                                                  (araneida::html-attr attrs))
                                        ("</~A>" (string-downcase (symbol-name tag))))))))
            (t (error "~A isn't a valid html tagoid." tagoid)))))
  t)

(defmacro tag ((tagoid &optional (stream '*default-tag-stream*)) &body body)
  `(invoke-tag ,(if (symbolp tagoid)
                    `(quote ,tagoid)
                    tagoid)
               ,stream (lambda () ,@body)))

(defun read-tag (s char prefix)
  (declare (ignore prefix char))
  (let ((next (read s t (values) t)))
    `(tag (,(car next)) ,@(cdr next))))