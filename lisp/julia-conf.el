;; -*- lexical-binding: t; -*-

(use-package julia-snail
  :straight t)

(use-package lsp-julia
  :straight t
  :hook (julia-mode . lsp)
  :init
  :config
  (setq lsp-julia-package-dir nil
        lsp-julia-flags (list
                         (format "-J%s"
                                 (expand-file-name "~/.install/languageserver.so")))))

(use-package julia-mode
  :straight t
  :config
  (require 'lsp-julia))

(provide 'julia-conf)
