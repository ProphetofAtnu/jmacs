;;; -*- lexical-binding: t; -*-

(defvar elixir-rpc-proc nil)
(defvar elixir-rpc-proc-name "elixir-rpc")
(defvar elixir-rpc-buffer-name "*elixir-rpc*")

(defvar *elixir-rpc-messages* nil)
(defvar *elixir-rpc-last-out* nil)

(defun elixir-rpc--decode-message (output)
  (condition-case nil 
      (if-let* ((trm (string-trim output))
                (out (unless (string-empty-p trm)
                       (base64-decode-string trm))))
          (prog1
              nil
            (push 
             (erlext-binary-to-term out)
             *elixir-rpc-messages*)))
    ('error output)))

(defun elixir-rpc--split-output (out)
  (cl-do* ((tstr out
                 (seq-drop tstr (+ p 1)))
           r 
           (p (string-search "\n" tstr)
              (string-search "\n" tstr)))
          ((null p) (nreverse r))
    (push (seq-take tstr (+ 1 p)) r)))


(defun elixir-rpc--process-filter (process output)
  (setq *elixir-rpc-last-out* (concat *elixir-rpc-last-out* output))
  (when (string-suffix-p "\n" *elixir-rpc-last-out*)
    (elixir-rpc--decode-message *elixir-rpc-last-out*)
    (setq *elixir-rpc-last-out* "")))

(defun elixir-rpc-start ()
  (unless (process-live-p elixir-rpc-proc)
    (setq elixir-rpc-proc
          (make-process
           :name "elixir-rpc"
           :buffer "*elixir-rpc-buffer*"
           :filter #'elixir-rpc--process-filter
           :command (list  (expand-file-name
                            "lisp/packages/eiex/mod/run-shell.sh"
                            user-emacs-directory)))))
  elixir-rpc-proc)

(defun elixir-rpc-stop ()
  (when (process-live-p elixir-rpc-proc)
    (kill-process elixir-rpc-proc)))

(defun elixir-rpc-encode (obj)
  (base64-encode-string (erlext-term-to-binary obj) t))

(defun elixir-rpc-send (obj)
  (process-send-string
   elixir-rpc-proc
   (concat
    (elixir-rpc-encode obj)
    "\n")))

(defun elixir-rpc-flush ()
  (let ((cur *elixir-rpc-messages*))
    (setq *elixir-rpc-messages* nil)
    cur))
