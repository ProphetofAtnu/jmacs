;;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'posframe)

(defun display-buffer-posframe (buffer alist)
  (let ((frame (posframe-show buffer
                              :border-color "gray"
                              :border-width 1
                              :position (point)
                              :poshandler 'posframe-poshandler-frame-top-center)))
    (set-frame-parameter frame 'close-on-quit t)))

(defvar popup-buffer-identifiers 
  '("\\*lsp-help\\*"
    "\\*Help\\*"
    "\\*Process List\\*"
    "\\*cider-doc\\*"
    "\\*helpful.*"))

(defvar popup-reusable-buffers
  '(help-mode
    cider-docview-mode
    compilation-mode
    process-menu-mode
    helpful-mode))

(defun build-bottom-buffer-params ()
  `((display-buffer-reuse-mode-window display-buffer-in-side-window)
    (window-parameters (close-on-quit . t))
    (side . bottom)
    (slot . -1)
    (mode . ,popup-reusable-buffers)
    (window-height . 0.25)))

(defun internal-build-popup-display-list () 
  (let ((bparams (build-bottom-buffer-params)))
    (mapcar (lambda
                (r)
              (cons r
                    bparams))
            popup-buffer-identifiers)))

(defvar literal-buffer-alist-entries
  '(("\\*eww\\*"
     (display-buffer-reuse-mode-window
      display-buffer-in-previous-window
      display-buffer-pop-up-window)
     (inhibit-same-window . t)
     (mode . eww-mode))
    ("\\*Embark Actions\\*"
     (display-buffer-posframe))))

(defun compile-buffer-display-alist ()
  (setq display-buffer-alist
        (append (internal-build-popup-display-list)
                literal-buffer-alist-entries)))

(add-hook 'emacs-startup-hook #'compile-buffer-display-alist)

(setq display-buffer-reuse-frames t)

(defun js/close-coq-windows-advice (&optional WIN)
  (mapc #'delete-window
        (-filter #'(lambda (w) (window-parameter w 'close-on-quit))
                 (window-list-1)))
  (mapc #'delete-frame
        (-filter #'(lambda (f)
                     (and  (frame-parameter f 'close-on-quit)
                           (frame-parameter f 'posframe-buffer)))
                 (frame-list))))

(advice-add 'keyboard-quit :before #'js/close-coq-windows-advice)

(provide 'window-conf)
