;;; -*- lexical-binding: t; -*-

(defvar eiex-comm-default-port 48771)
(defvar eiex-comm-proc-name "eiex")
(defvar eiex-comm-buffer-name "*eiex-comm*")


(defun eiex-comm--process-filter (pr data)
  (with-current-buffer (process-buffer pr)
    (goto-char (point-max))
    (let ((str (etf-unpack (string-to-unibyte data))))
      (prin1 str (current-buffer))
      (insert "\n")
      (prin1 (etf-convert str) (current-buffer))
      (insert "\n"))))

(defun eiex-comm--startup ()
  (let ((p (open-network-stream
            eiex-comm-proc-name
            eiex-comm-buffer-name
            "localhost" eiex-comm-default-port)))
    (set-process-filter p 'eiex-comm--process-filter)
    (set-process-filter-multibyte p nil)
    (with-current-buffer (process-buffer p)
      (set-buffer-multibyte nil))))

(provide 'eiex-comm)
