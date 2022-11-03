;; -*- lexical-binding: t; -*-

(use-package zig-mode
    :straight t
    :config
    (setq zig-format-on-save nil))

(use-package lsp-mode
    :straight t
    :hook (zig-mode . lsp)
    :config
    (setq lsp-zig-zls-executable (expand-file-name "~/.zls/zls")))

;; (use-package lsp-bridge
;;     :straight t
;;     :hook (zig-mode . lsp-bridge-mode))

(provide 'zig-conf)
