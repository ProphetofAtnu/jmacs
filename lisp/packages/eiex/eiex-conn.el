;;; -*- lexical-binding: t; -*-

(defvar eiex-comm-default-port 48771)
(defvar eiex-comm-proc-name "eiex")
(defvar eiex-comm-buffer-name "*eiex-comm*")
(defvar eiex-comm-node-name "eiex@localhost")

(defvar *eiex-comm--pending-cbs* nil)
(defvar *eiex-comm--ref-seq* 0)

(defvar eiex-comm--server-connectable nil)

(defvar *eiex-comm-proc* nil)

(defvar *eiex-comm-inbox* '())

(defun eiex-comm-flush ()
  (prog1
      (let ((c *eiex-comm-inbox*))
        c)
    (setq *eiex-comm-inbox* '())))

(defun eiex-comm--server-filter (proc data)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert data)
    (cond
      ((string-match-p
        "^Accepting connections on .*" data)
       (progn (setq eiex-comm--server-connectable t))))))

(defun eiex-comm-run ()
  (setq eiex-comm--server-connectable nil)
  (make-process
   :name "eiex-server"
   :buffer "*eiex-server-log*"
   :filter #'eiex-comm--server-filter
   :command (list
             (expand-file-name
              "lisp/packages/eiex/mod/run-shell.sh"
              user-emacs-directory))))

(defun eiex-comm--process-filter (pr data)
  (with-current-buffer (process-buffer pr)
    (let ((d (etf-binary-to-term data)))
      (goto-char (point-max))
      (insert (format "%s: %s\n" (current-time-string) d))
      (if (length> *eiex-comm--pending-cbs* 0)
          (funcall (pop *eiex-comm--pending-cbs*)
                   d)
        (push d *eiex-comm-inbox*)))))

(defun eiex-comm--startup ()
  (let ((p (open-network-stream
            eiex-comm-proc-name
            eiex-comm-buffer-name
            "localhost" eiex-comm-default-port)))
    (set-process-filter p 'eiex-comm--process-filter)
    (set-process-filter-multibyte p nil)
    (with-current-buffer (process-buffer p)
      (set-buffer-multibyte nil))
    p))

(defun eiex-comm-get-proc ()
  (unless (process-live-p *eiex-comm-proc*)
    (setq *eiex-comm-proc* (eiex-comm--startup)))
  *eiex-comm-proc*)

(defun eiex-comm-call (term &optional timeout)
  (let ((proc (eiex-comm-get-proc))
        (res nil))
    (push #'(lambda (x)
              (setf res x))
          *eiex-comm--pending-cbs*)
    (process-send-string proc
                         (etf-term-to-binary term))
    (with-local-quit 
      (when (accept-process-output proc timeout)
        res))))

(defun eiex-comm-cast (term)
  (process-send-string (eiex-comm-get-proc)
                       (etf-term-to-binary term)))

(defun eiex-comm-complete (str)
  (eiex-comm-call (vector 'complete str)))

(provide 'eiex-comm)
