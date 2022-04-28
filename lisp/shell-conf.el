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
    :init
    (add-hook 'shell-dynamic-complete-functions #'bash-completion-dynamic-complete))

(use-package vterm
    :straight t)

(use-package vterm-toggle
    :straight t)

(provide 'shell-conf)

