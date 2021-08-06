;;; -*- lexical-binding: t; -*-

(use-package lsp-mode
    :straight t
    :defer t)

(use-package lsp-ui
    :straight t
    :hook (lsp-mode . lsp-ui-mode))


(provide 'lsp-conf)
