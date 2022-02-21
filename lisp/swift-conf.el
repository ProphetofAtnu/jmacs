;;; -*- lexical-binding: t; -*-

(use-package lsp-sourcekit
    :straight t
    :hook (swift-mode . lsp-deferred))

(use-package swift-mode
    :straight t)

(provide 'swift-conf)
