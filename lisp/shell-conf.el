;;; -*- lexical-binding: t; -*-

(require 'general)
(require 'use-package)

;; (setq system-uses-terminfo nil)
;; (setq explicit-shell-file-name "/usr/local/bin/bash")

(use-package shell-pop
  :straight t
  :config
  (setq shell-pop-full-span t))
  
(use-package company
    :hook ((eshell-mode shell-mode) . 'company-mode))

(provide 'shell-conf)
