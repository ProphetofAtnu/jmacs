;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)

(use-package clojure-mode
  :straight t
  :defer t)

(use-package cider
  :straight t
  :after (clojure-mode)
  :config
  (add-hook 'cider-repl-mode-hook 'company-mode))

(provide 'clojure-conf)
