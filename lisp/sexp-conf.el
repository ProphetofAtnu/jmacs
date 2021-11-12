;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)
(require 'evil)

;; (use-package paredit
;;   :straight t)

;; (use-package parinfer
;;     :straight t
;;     :general
;;     (:keymaps 'parinfer-mode-map
;;               "S-<tab>" 'parinfer-smart-tab:dwim-left
;;               "<tab>" 'parinfer-smart-tab:dwim-right-or-complete)
;;     (local-leader-def
;;         :keymaps '(lisp-mode-shared-map)
;;       "p" 'parinfer-toggle-mode)
;;     :init
;;     (progn
;;       (setq parinfer-extensions
;;             '(defaults       ; should be included.
;;               pretty-parens  ; different paren styles for different modes.
;;               evil
;;               paredit
;;               smart-yank))   ; Yank behavior depend on mode.
;;       (add-hook 'clojure-mode-hook #'parinfer-mode)
;;       (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;;       (add-hook 'common-lisp-mode-hook #'parinfer-mode)
;;       (add-hook 'scheme-mode-hook #'parinfer-mode)
;;       (add-hook 'lisp-mode-hook #'parinfer-mode)))

(use-package lispy
    :straight t
    :hook ((emacs-lisp-mode
            lisp-mode
            clojure-mode
            common-lisp-mode
            scheme-mode)
           . lispy-mode)
    :config
    (setq lispy-completion-method 'default
          lispy-close-quotes-at-end-p t))

(use-package lispyville
    :straight t
    :hook (lispy-mode . lispyville-mode))

(provide 'sexp-conf)
