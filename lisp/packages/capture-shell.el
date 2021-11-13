;;; -*- lexical-binding: t; -*-

(require 'dash)

(defvar capture-shell-last-command nil)

(defvar capture-shell-history-file (expand-file-name
                                    "var/capture-shell-hist"
                                    user-emacs-directory))

(defun capture-shell--capture-shell-command (cmd &rest args)
  (setq capture-shell-last-command
        cmd)
  (with-temp-buffer
    (insert cmd)
    (newline)
    (append-to-file
     (point-min)
     (point-max)
     capture-shell-history-file))
  nil)

(defun rerun-async-command ()
  (interactive)
  (progn 
    (with-demoted-errors
        (let ((kill-buffer-query-functions nil))
          (kill-buffer (get-buffer "*Async Shell Command*"))))
    (funcall-interactively 'async-shell-command
                           capture-shell-last-command)))

(defun capture-shell--uniq (l)
  (let (u)
    (dolist (x l (reverse u))
      (unless (memq x u)
        (push x u)))))

(defun rerun-pick-async-command ()
  (interactive)
  (let ((old-commands (with-temp-buffer
	                (insert-file-contents capture-shell-history-file)
                        (capture-shell--uniq  (reverse (split-string (buffer-string) "\n" t))))))
    (async-shell-command (completing-read "Rerun: " old-commands))))

(define-minor-mode capture-shell-command-mode
    "Minor mode for capturing async commands"
  :keymap (define-keymap
              (kbd "C-&") 'rerun-async-command
            (kbd "C-M-&") 'rerun-pick-async-command)
  :global t
  (if capture-shell-command-mode
      (advice-add 'async-shell-command :after #'capture-shell--capture-shell-command)
    (advice-remove 'async-shell-command #'capture-shell--capture-shell-command)))

(provide 'capture-shell)
