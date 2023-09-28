;;; -*- lexical-binding: t; -*-

;; (use-package eglot
;;     :straight t

;;     :hook (swift-mode . eglot-ensure)
;;     :config
;;     (add-to-list 'eglot-server-programs
;;                  '(swift-mode "sourcekit-lsp")))

(straight-use-package 'lsp-sourcekit)

(use-package swift-mode
    :straight t
    :mode ("\\.swift\\'" . swift-mode)
    :config
    (require 'projectile)
    (add-to-list 'projectile-project-root-files
                 "Package.swift")
    (require 'lsp-sourcekit)
    (add-to-list 'lsp-file-watch-ignored-directories
		 "[/\\\\]\\.build\\'"))

;; (use-package lsp-sourcekit
;;     :straight t)

(use-package lsp-mode
  :straight t
  :hook (swift-mode . lsp-deferred))

(provide 'swift-conf)
