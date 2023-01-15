;;; -*- lexical-binding: t; -*-

(use-package glsl-mode
  :straight t)

(use-package cmake-mode
  :straight t
  :init
  (setenv "CMAKE_EXPORT_COMPILE_COMMANDS" "1"))

;; (use-package ccls
;;   :straight t
;;   )

(use-package lsp-mode
  :straight t
  :hook ((cmake-mode objc-mode c-mode c++-mode) . lsp-deferred)
  :general
  (local-leader-def
    :keymaps '(c++-mode-map c-mode-map objc-mode-map)
    "o" 'lsp-clangd-find-other-file)
  :config
  (setq lsp-lens-enable nil)
  (remove-hook 'lsp-mode-hook #'lsp-lens-mode)
  (require 'lsp-clangd)
  (setq lsp-clients-clangd-args
        (append lsp-clients-clangd-args
                '("--all-scopes-completion"
                                        ; "--completion-style=detailed"
                  "--background-index"
                  "-j=8"
                  "--compile-commands-dir=build"
                  "--pch-storage=memory"))))

(use-package cc-mode
  :config
  (let ((cur (alist-get "java" c-style-alist
                        nil nil #'string=)))
    (setf (alist-get 'c-basic-offset cur) 8)))

(use-package qml-mode
  :straight t)

(defconst cmake-export-compile-commands-flag
  "-DCMAKE_EXPORT_COMPILE_COMMANDS=1")

(add-to-list 'auto-mode-alist
	     '("\\.mm\\'" . objc-mode))

(provide 'c-conf)
