;;; -*- lexical-binding: t; -*-

(require 'use-package)

(require 'general)

(use-package sly
    :straight t
    :hook (lisp-mode . sly-mode)
    :general
    (local-leader-def
        :keymaps '(lisp-mode-map)
      "," 'sly
      "q" 'sly-reset
      "Q" 'sly-quit-lisp
      "a" 'sly-apropos-all
      "x" 'sly-scratch
      "r" 'sly-pprint-eval-region
      "e" 'sly-pprint-eval-last-expression
      "b" 'sly-eval-buffer
      "d" 'sly-cd
      "D" 'sly-pwd)
    :config 
    (setq sly-contribs '(sly-mrepl
                         sly-fancy
                         sly-tramp
                         sly-autodoc
                         sly-scratch
                         sly-stickers
                         sly-package-fu
                         sly-fancy-trace
                         sly-indentation
                         sly-trace-dialog
                         sly-fontifying-fu
                         sly-fancy-inspector)))





(provide 'clisp-conf)
