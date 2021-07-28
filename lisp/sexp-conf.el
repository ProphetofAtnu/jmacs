;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)
(require 'evil)

(use-package paredit
  :straight t)

(use-package parinfer
  :straight t
  :general
  (local-leader-def
   :keymaps '(lisp-mode-shared-map)
   "p" 'parinfer-toggle-mode
   )
  
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             pretty-parens  ; different paren styles for different modes.
             evil
             paredit
             smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))


(provide 'sexp-conf)
