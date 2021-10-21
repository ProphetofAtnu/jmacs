;;; -*- lexical-binding: t; -*-

(use-package lsp-mode
    :straight t
    :defer t)

(use-package lsp-ui
    :straight t
    :hook (lsp-mode . (lambda ()
                        (lsp-ui-mode)
                        (setq-local evil-lookup-func 'lsp-describe-thing-at-point))))

(provide 'lsp-conf)
