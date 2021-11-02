;;; -*- lexical-binding: t; -*-

(use-package js2-mode
    :straight t
    :mode (("\\.js\\'" . js2-mode)
           ("\\.jsx\\'" . js2-jsx-mode))
    :general
    (:keymaps 'js2-mode-map
              :modes 'insert
              "RET" 'js2-line-break))

(use-package typescript-mode
    :straight t)

(use-package lsp-mode
    :straight t
    :hook ((typescript-mode
            json-mode
            js2-mode
            js2-jsx-mode)
           . lsp-deferred))

(use-package web-mode
    :straight t)

(use-package json-mode
    :straight t)

(use-package emmet-mode
    :straight t
    :hook (html-mode . emmet-mode))

(provide 'web-conf)
