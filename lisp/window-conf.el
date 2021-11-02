;;; -*- lexical-binding: t; -*-

(require 'dash)

(setq display-buffer-alist
      '(("\\*lsp-help\\*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-parameters (close-on-quit . t))
         (window-height . 0.25))
        ("\\*Help\\*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-parameters (close-on-quit . t))
         (window-height . 0.25))
        ("\\*Process List\\*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-parameters (close-on-quit . t))
         (window-height . 0.25))
        ("\\*helpful.*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-parameters (close-on-quit . t))
         (window-height . 0.25))))

(defun js/close-coq-windows-advice (&optional WIN)
  (mapc #'delete-window
        (-filter #'(lambda (w) (window-parameter w 'close-on-quit))
                 (window-list-1))))

(advice-add 'keyboard-quit :before #'js/close-coq-windows-advice)

(provide 'window-conf)
