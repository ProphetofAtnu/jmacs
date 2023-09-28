;; -*- lexical-binding: t; -*-

(use-package fsharp-mode
  :straight t
  :mode ("\\.fs[iylx]?\\'" . fsharp-mode)
  )

(use-package csharp-mode
  :straight t
  :mode ("\\.cs\\'" . csharp-mode)
  )

(use-package lsp-csharp
    :hook (csharp-mode . lsp-deferred)
    :config
    (setq lsp-csharp-server-path
	  (expand-file-name
	   "OmniSharp"
	   lsp-csharp-omnisharp-roslyn-server-dir)))

(use-package powershell
  :straight t
  :mode ("\\.ps[dm]?1\\'" . powershell-mode))

(provide 'dotnet-conf)
