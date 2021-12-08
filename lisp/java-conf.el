;;; -*- lexical-binding: t; -*-

(use-package lsp-java
    :straight t
    :hook (java-mode . lsp-deferred))

(provide 'java-conf)
