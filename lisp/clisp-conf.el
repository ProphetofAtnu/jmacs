;;; -*- lexical-binding: t; -*-

(require 'use-package)

(require 'general)

(use-package sly
  :straight t
  :hook (lisp-mode . sly-mode))


(provide 'clisp-conf)
