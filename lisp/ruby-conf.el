;;; -*- lexical-binding: t; -*-
;; Notes
;; see robe
;; (add-to-list 'company-backends
;;              (cape-company-to-capf 'company-robe))

(use-package lsp-mode
    :straight t
    :hook (ruby-mode . lsp-deferred))

(provide 'ruby-conf)
