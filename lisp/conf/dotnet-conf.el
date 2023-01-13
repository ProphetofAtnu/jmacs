;; -*- lexical-binding: t; -*-

(use-package fsharp-mode
    :straight t)

(use-package csharp-mode
    :straight t)

(use-package lsp-csharp
    :hook (csharp-mode . lsp-deferred)
    :config
    (setq lsp-csharp-server-path
	  (expand-file-name
	   "OmniSharp"
	   lsp-csharp-omnisharp-roslyn-server-dir)))

(use-package powershell
    :straight t)

(provide 'dotnet-conf)
