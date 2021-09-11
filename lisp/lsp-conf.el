;;; -*- lexical-binding: t; -*-

(use-package lsp-mode
    :straight t
    :defer t)

(use-package lsp-ui
    :straight t
    :hook (lsp-mode . (lambda ()
                        (lsp-ui-mode)
                        (setq-local evil-lookup-func 'lsp-describe-thing-at-point)))
    :config
    (add-to-list 'display-buffer-alist
                 '("\\*lsp-help\\*" .
                   (display-buffer-at-bottom .
                    ((window-parameters . ((close-on-quit t
                                                          (window-height . 0.25)))))))))

(provide 'lsp-conf)
