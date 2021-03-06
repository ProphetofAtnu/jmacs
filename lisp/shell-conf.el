;;; -*- lexical-binding: t; -*-

(require 'general)
(require 'use-package)

;; (setq system-uses-terminfo nil)
;; (setq explicit-shell-file-name "/usr/local/bin/bash")

(use-package shell-pop
  :straight t
  :general
  (override
   "C-'" 'shell-pop)
  :config
  (setq shell-pop-full-span t))
  
;; (use-package company
;;     :hook ((eshell-mode shell-mode) . 'company-mode)
;;     )

;; (use-package eshell
;;     :config
;;   (add-hook 'eshell-mode-hook (l)))

(use-package bash-completion
    :straight t
    :commands (bash-completion-dynamic-complete-nocomint)
    :init
    (add-hook 'shell-dynamic-complete-functions #'bash-completion-dynamic-complete))

(use-package vterm
    :straight t
    :commands (vterm vterm-other-window))

(defun shell-scratch-create ()
  (interactive)
  (let ((sh-buf (get-buffer-create (generate-new-buffer "*shell-scratch*"))))
    (with-current-buffer sh-buf
      (sh-mode))
    (pop-to-buffer sh-buf)))

(use-package bash-comp-capf
    :hook (sh-mode-hook . bash-comp-capf-setup))

(with-eval-after-load 'vterm
  (defun vterm-send-region-to-shell (beg end)
    (interactive "r")
    (let* ((sub (buffer-substring-no-properties beg end))
           (vterm (match-buffers '(major-mode . vterm-mode)))
           (vterm-buf (car vterm)))
      (with-current-buffer vterm-buf
        (vterm-send-string (if (string-suffix-p "\n" sub)
                               sub
                             (concat sub "\n"))
                           t))))

  (defun vterm-send-line-to-shell (beg end)
    (interactive "r")
    (vterm-send-region-to-shell (point-at-bol) (point-at-eol)))

  (evil-define-operator js/evil-vterm-delete-forward-char (beg end type register)
    "Delete previous character."
    :motion evil-forward-char
    (interactive "<R><x>")
    (evil-collection-vterm-delete beg end type register))
  
  (local-leader-def
      :keymaps '(sh-mode-map)
    "v" '(:ignore t :wk "Vterm")
    "v r" 'vterm-send-region-to-shell
    "v l" 'vterm-send-line-to-shell
    )

  (general-defs
      :keymaps 'vterm-mode-map
    :states '(normal visual)
    "x" 'js/evil-vterm-delete-forward-char)
  )

(use-package vterm-toggle
    :straight t
    :commands (vterm-toggle))

(provide 'shell-conf)

