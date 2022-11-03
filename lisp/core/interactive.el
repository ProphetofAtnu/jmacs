;; -*- lexical-binding: t; -*-
(require 'core/utility)

(defun debugging-set-region-props (beg end)
  (interactive "r")
  (minibuffer-with-setup-hook (lambda ()
                                (insert "(")
                                (save-excursion
                                  (insert ")")))
    (let ((inhibit-modification-hooks)
          (pl (read-minibuffer "PLIST: ")))
      (set-text-properties beg end pl)
      (message "Fontified from %d to %d" beg end))))

(defun debugging-add-region-props (beg end)
  (interactive "r")
  (minibuffer-with-setup-hook (lambda ()
                                (insert "(")
                                (save-excursion
                                  (insert ")")))
    (let ((inhibit-modification-hooks t)
          (pl (read-minibuffer "PLIST: ")))
      (add-text-properties beg end pl)
      (message "Fontified from %d to %d" beg end))))

(defun debugging-add-region-face (beg end)
  (interactive "r")
  (let ((inhibit-modification-hooks t)
        (pl (read-face-name "Face: ")))
    (alter-text-property beg end 'face (lambda (fce) (cons pl fce)))
    (message "Added %s from %d to %d" pl beg end)))


(defun ielm-pop-with-current-buffer ()
  (interactive)
  (let ((cur (current-buffer))
        (il (save-window-excursion
              (ielm "*ielm*")
              (get-buffer "*ielm*"))))
    (with-current-buffer
        il
      (ielm-change-working-buffer
       cur))
    (pop-to-buffer il
     '((display-buffer-reuse-window display-buffer-at-bottom)
       (direction . below)
       (window-height . 0.2)))))

(provide 'core/interactive)
