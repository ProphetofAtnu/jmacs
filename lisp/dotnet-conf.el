;; -*- lexical-binding: t; -*-

(use-package fsharp-mode
    :straight t
    :config
    (add-to-list 'eglot-server-programs '(fsharp-mode . ("fsautocomplete"))))

(use-package csharp-mode
    :straight t)

(use-package lsp-mode
    :straight t)

(provide 'dotnet-conf)
