;; -*- lexical-binding: t; -*-

;; Shut up byte comp
(autoload 
  'bash-completion-dynamic-complete-nocomint "bash-completion")

(defun bash-comp--ll-escaped-p ()
  (save-excursion
    (and  (zerop (forward-line -1))
          (eq ?\\ (char-before (point-at-eol))))))

(defun bash-comp--last-nonesc ()
  (save-excursion
    (goto-char (point-at-bol))
    (while (bash-comp--ll-escaped-p)
      (forward-line -1))
    (point)))

(defun bash-completion-capf ()
  (bash-completion-dynamic-complete-nocomint
   (bash-comp--last-nonesc)
   (point) t))

(defun bash-comp-capf-setup ()
  (add-hook 'completion-at-point-functions #'bash-completion-capf nil t))

(provide 'bash-comp-capf)
