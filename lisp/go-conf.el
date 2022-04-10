;;; -*- lexical-binding: t; -*-

(use-package go-mode
    :straight t)

(use-package lsp-mode
    :straight t
    :hook (go-mode . lsp-deferred))

(provide 'go-conf)
