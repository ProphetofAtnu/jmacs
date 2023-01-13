;; -*- lexical-binding: t; -*-

(use-package all-the-icons
  :straight t)

(use-package telephone-line
  :straight t
  :config
  (delight 'emacs-lisp-mode "Elisp")
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (setq telephone-line-height 24
        telephone-line-evil-use-short-tag t)
  (telephone-line-mode +1))

(use-package kaolin-themes
  :straight t
  :defer t)

(use-package cherry-blossom-theme
  :straight t
  :defer t)

(use-package modus-themes
  :straight t
  :config
  (load-theme 'modus-vivendi t))

(provide 'global-theme)
