;; -*- lexical-binding: t; -*-

(use-package julia-snail
  :straight t
  :after (julia-mode))

(use-package lsp-julia
  :straight t
  :hook (julia-mode . lsp)
  :config
  (setq lsp-julia-package-dir nil
        lsp-julia-flags (list
                         (format "-J%s"
                                 (expand-file-name "~/.install/languageserver.so")))))

(use-package julia-mode
  :straight t
  :mode ("\\.jl\\'" . julia-mode)
  :config
  (require 'lsp-julia))

(provide 'julia-conf)
