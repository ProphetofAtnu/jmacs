;;; -*- lexical-binding: t; -*-

(use-package glsl-mode
  :straight t
  :defer t)

(use-package cmake-mode
  :straight t
  :defer t
  :init
  (setenv "CMAKE_EXPORT_COMPILE_COMMANDS" "1"))

;; (use-package ccls
;;   :straight t
;;   )

(use-package meson-mode
  :straight t
  :defer t)

(use-package thrift-mode
  :straight t
  :defer t)

;; (use-package python
;;   :defer t
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.py[iw]?\\'" . python-ts-mode))
;;   (add-to-list 'interpreter-mode-alist '("python[0-9.]*" . python-ts-mode)))

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
  )

(use-package lsp-clangd
  :after (lsp-mode)
  :config
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
  :general
  (general-defs
    :keymaps '(c-mode-base-map ;; Should be enough to fix capf, buf I'm the ones that give me issues too.
               c-mode-map c++-mode-map
               java-mode-map objc-mode-map)
    :states '(insert emacs)
    "TAB" 'indent-for-tab-command
    )
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
