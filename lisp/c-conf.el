;;; -*- lexical-binding: t; -*-

(use-package glsl-mode
    :straight t)

(use-package cmake-mode
    :straight t)

(use-package lsp-mode
    :straight t
    :hook ((objc-mode c-mode c++-mode) . lsp-deferred))

(use-package acm
    :straight t)

;; (use-package lsp-bridge
;;     :straight t
;;     :hook ((objc-mode c-mode c++-mode) . lsp-bridge-mode))

(defconst cmake-export-compile-commands-flag
  "-DCMAKE_EXPORT_COMPILE_COMMANDS=1")

(provide 'c-conf)
