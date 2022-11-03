;;; -*- lexical-binding: t; -*-

(use-package lsp-java
    :straight t
    :hook (java-mode . lsp-deferred))

(use-package kotlin-mode
    :straight t
    :config
    (add-to-list 'exec-path
                 (expand-file-name "var/lsp/server/kotlin/server/bin/" user-emacs-directory)))


(provide 'java-conf)
