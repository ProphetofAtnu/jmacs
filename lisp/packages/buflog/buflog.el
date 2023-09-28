;; -*- lexical-binding: t; -*-

(defun buflog--default-prefix-fn (&rest _args)
  (let ((pre "LOG: "))
    (set-text-properties 0 4 '(face highlight) pre)
    (insert pre)))

(defvar buflog-fallback-name "*buflog-general*")

(defvar *buflog-output* nil)

(defvar-local buflog-want-display-on-log t)

(defvar-local buflog-prefix #'buflog--default-prefix-fn
  "If non-nil, a function or string to use as a prefix in logging output.

When buflog-prefix is a function, it is run as if by with-buflog-output.")


(defun buflog-get-or-create (name-or-buffer)
  (let ((b (get-buffer-create name-or-buffer)))
    (unless (eq (buffer-local-value 'major-mode b)
                'buflog-output-mode)
      (with-current-buffer b
        (buflog-output-mode)))
    b))



(defmacro with-buflog-output (&rest body)
  "Run in the context of the buflog output buffer, setting the point to point-max."
  `(with-current-buffer (or *buflog-output*
                            (buflog-get-or-create buflog-fallback-name))
     (let ((inhibit-read-only t))
       (goto-char (point-max))
       (combine-after-change-calls 
         ,@body))))

(defun buflog-log (str &rest props)
  (with-buflog-output
   (cl-typecase buflog-prefix
     (string (insert string))
     (function (funcall buflog-prefix props)))
   (insert str)
   (newline)
   (when buflog-want-display-on-log
     (display-buffer (current-buffer))))
  nil)

(defun buflog-format (format-string &rest args)
  (buflog-log (apply #'format format-string args)))

(defun buflog-format-with-props (props format-string &rest args)
  (cl-assert (listp props))
  (apply #'buflog-log (apply #'format format-string args)
         props))

(defun buflog--enable-output-mode ())

(define-derived-mode buflog-output-mode special-mode "Buflog Output"
  "Major mode for buflog buffers"
  (buflog--enable-output-mode))

(provide 'buflog)
