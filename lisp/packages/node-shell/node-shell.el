;;; -*- lexical-binding: t; -*-


(defvar node-shell--compl-proc nil)
(defvar-local node-shell--read-marker nil)
(defvar-local node-shell--last-write-start nil)

(defun node-shell-start ()
  (make-comint-in-buffer
   "*node-shell*"
   "node-shell"
   "node"
   nil
   "/home/scaggj/.emacs.d/lisp/packages/node-shell/shell.js"))

(defun node-shell--process-filter (proc input)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (setq-local node-shell--last-write-start (point-marker))
    (insert input)))

(defun node-shell--connect-completions ()
  (when (and
         node-shell--compl-proc
         (process-live-p node-shell--compl-proc))
    (kill-process node-shell--compl-proc))
  (setq node-shell--compl-proc
        (make-network-process
         :name "node-shell-complete"
         :buffer "*node-shell-completions*"
         :host 'local
         :service 46623
         :filter #'node-shell--process-filter))
  (node-shell--setup-buffer))

(defun node-shell--setup-buffer ()
  (with-current-buffer (process-buffer node-shell--compl-proc)
    (setq-local node-shell--read-marker
                (point-min-marker))))

(defun node-shell--complete-string-send (line)
  (process-send-string node-shell--compl-proc line))

(defun node-shell--read-next-object ()
  (with-current-buffer (process-buffer node-shell--compl-proc)
    (goto-char node-shell--read-marker)
    (prog1
        (ignore-errors
          (json-parse-buffer :object-type 'plist))
      (setq-local node-shell--read-marker (point-marker)))))

(defun node-shell--read-last-object ()
  (with-current-buffer (process-buffer node-shell--compl-proc)
    (goto-char node-shell--last-write-start)
    (prog1
        (ignore-errors
          (json-parse-buffer :object-type 'plist))
      (setq-local node-shell--read-marker (point-marker)))))

(defun node-shell--clear-complete-buffer ()
  (with-current-buffer (process-buffer node-shell--compl-proc)
    (erase-buffer)))


(defun node-shell--debug-read-marks ()
  (with-current-buffer (process-buffer  node-shell--compl-proc)
    (list  :point (point)
           :read-marker node-shell--read-marker
           :last-write-marker node-shell--last-write-start)))

(defun company-node-shell (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case
      (interactive (company-begin-backend 'company-node-shell))
    (prefix ( ))
    ))

(provide 'node-shell)
