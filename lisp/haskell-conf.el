;;; -*- lexical-binding: t; -*-

(use-package haskell-mode
    :straight t
    )

(use-package dante
    :straight t)

(use-package lsp-haskell
    :straight t)

(use-package lsp-mode
    :straight t
    :hook ((haskell-mode haskell-literate-mode) . lsp-deferred))

(provide 'haskell-conf)
