;;; -*- lexical-binding: t; -*-

(use-package go-mode
    :straight t
    :config
    (add-hook 'go-mode-hook
              (lambda ()
                (setq-local tab-width 4))))

(use-package go-impl
    :straight t
    :general
    (local-leader-def
        'go-mode-map
        "i" 'go-impl))

(use-package go-tag
    :straight t
    :general
    (local-leader-def
        'go-mode-map
        "t" 'go-tag-add
        "T" 'go-tag-remove))

(use-package lsp-mode
    :straight t
    :hook (go-mode . lsp-deferred))

(provide 'go-conf)
