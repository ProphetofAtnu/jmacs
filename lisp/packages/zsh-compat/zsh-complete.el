;;; -*- lexical-binding: t; -*-

(defvar zsh-capture-script (expand-file-name
                            "deps/zsh-capture-completion/capture.zsh"
                            user-emacs-directory))

(defun cleanup-lines )

(defun zsh-simple-candidates (prefix)
  (when (stringp prefix)
    (process-lines zsh-capture-script prefix)))

(defun async-zsh-candidates (prefix)
  (let ((buf (get-buffer-create "*zsh-candidates*")))
    (with-current-buffer buf
      (erase-buffer))
    (make-process :name "zsh-candidates" :buffer buf
                  :command (list zsh-capture-script prefix)
                  :sentinel #'ignore
                  )))

(defun ansi-insertion-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert (ansi-color-filter-apply
                   (string-replace "\x0D" "" string)))
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(setq *iex*

      (make-process :name "iex" :buffer (get-buffer-create "*runtime*")
                    :command (list "/usr/local/bin/iex")
                    ;; :filter #'ansi-insertion-filter
                    ))

(with-current-buffer (process-buffer *iex*)
  (process-send-string *iex* "IO.pu")
  (process-send-string *iex* "\t"))

(with-current-buffer (process-buffer *iex*)
  (process-send-eof *iex* ))

(kill-process *iex*)
