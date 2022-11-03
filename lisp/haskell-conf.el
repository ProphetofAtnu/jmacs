;;; -*- lexical-binding: t; -*-

(use-package haskell-mode
    :straight t
    :general
    (local-leader-def
        :keymaps 'haskell-mode-map
      "," 'haskell-interactive-switch
      "i" 'haskell-add-import
      "I" 'haskell-align-imports
      "r" 'haskell-process-reload
      "c" 'haskell-check
      ))

;; (use-package dante
;;     :straight t
;;     :hook (haskell-mode . dante-mode)
;;     :config
;;     (defun dante--wrap-company ()
;;       (add-hook 'completion-at-point-functions (cape-company-to-capf #'dante-company) nil t))
    
;;     (add-hook 'dante-mode-hook
;;               #'company-mode))

(use-package lsp-haskell
    :straight t)

(use-package lsp-mode
    :straight t
    :hook ((haskell-mode haskell-literate-mode) . lsp-deferred))

(provide 'haskell-conf)
