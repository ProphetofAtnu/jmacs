;;; -*- lexical-binding: t; -*-

(require 'dash)

(defvar popup-buffer-identifiers 
  '("\\*lsp-help\\*"
    "\\*Help\\*"
    "\\*Process List\\*"
    "\\*helpful.*"))

(defvar popup-reusable-buffers
  '(help-mode
    compilation-mode
    process-menu-mode
    helpful-mode))

(defun build-bottom-buffer-params ()
  `((display-buffer-reuse-mode-window display-buffer-at-bottom)
    (window-parameters (close-on-quit . t))
    (mode . ,popup-reusable-buffers)
    (window-height . 0.25)))

(defun internal-build-popup-display-list () 
  (let ((bparams (build-bottom-buffer-params)))
    (mapcar (lambda
                (r)
              (cons r
                    bparams))
            popup-buffer-identifiers)))

(defun compile-buffer-display-alist ()
  (setq display-buffer-alist
        (internal-build-popup-display-list)))

(add-hook 'emacs-startup-hook #'compile-buffer-display-alist)

(defun js/close-coq-windows-advice (&optional WIN)
  (mapc #'delete-window
        (-filter #'(lambda (w) (window-parameter w 'close-on-quit))
                 (window-list-1))))

(advice-add 'keyboard-quit :before #'js/close-coq-windows-advice)

(provide 'window-conf)
