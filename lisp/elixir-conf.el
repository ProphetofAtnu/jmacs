;;; -*- lexical-binding: t; -*-

(use-package elixir-mode
    :straight t)

(use-package lsp-mode
    :straight t
    :commands lsp
    :ensure t
    :diminish lsp-mode
    :hook
    (elixir-mode . lsp)
    :init
    (add-to-list 'exec-path "path-to-elixir-ls/release"))

(provide 'elixir-conf)
