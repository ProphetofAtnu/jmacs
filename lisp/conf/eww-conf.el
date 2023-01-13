;;; -*- lexical-binding: t; -*-

(defun eww-find-other-buffer ()
  (let (r)
    (dolist (buf (buffer-list) r)
      (when (and (not (eq (current-buffer) buf))
                 (eq (buffer-local-value 'major-mode buf)
                     'eww-mode))
        (setf r buf)))))

(defun eww-get-other-session ()
  (or
   (eww-find-other-buffer)
   (with-current-buffer
       (generate-new-buffer "*eww*")
     (eww-mode)
     (current-buffer))))

(defun eww-open-in-other-buffer ()
  (interactive)
  (let ((curl (eww--url-at-point))
        (b (eww-get-other-session)))
    (save-window-excursion
      (with-current-buffer b
        (eww-browse-url curl)))
    (display-buffer b)))

(use-package eww
    :commands (eww eww--url-at-point eww-browse-url)
    :general (:keymaps 'eww-mode-map
                       "C-<return>" 'eww-open-in-other-buffer
                       "C-RET" 'eww-open-in-other-buffer))


(provide 'eww-conf)
