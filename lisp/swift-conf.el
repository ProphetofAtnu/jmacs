;;; -*- lexical-binding: t; -*-

;; (use-package lsp-sourcekit
;;     :straight t
;;     :hook (swift-mode . lsp-deferred))

(use-package eglot
    :straight t

    :hook (swift-mode . eglot-ensure)
    :config
    (add-to-list 'eglot-server-programs
                 '(swift-mode "sourcekit-lsp")))

(use-package swift-mode
    :straight t
    :config
    (require 'projectile)
    (add-to-list 'projectile-project-root-files
                 "Package.swift"))

(provide 'swift-conf)
