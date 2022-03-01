;;; -*- lexical-binding: t; -*-

(use-package lsp-mode
    :straight t
    :defer t
    :hook (lsp-mode . lsp-lens-mode)
    :general
    :config
    (local-leader-def
        :definer 'minor-mode
      :keymaps '(lsp-mode)
      "." 'lsp-execute-code-action
      "*" 'lsp-restart-workspace
      "/" 'lsp-avy-lens
      )
    (general-defs
        :keymaps 'prefix-utility-map
      "s" 'lsp)
    ;; Uncomment following section if you would like to tune lsp-mode performance according to
    ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
    (setq lsp-idle-delay 0.500)
    (setq lsp-log-io nil)
    (setq lsp-completion-provider :none)
    (setq lsp-prefer-flymake nil))

(use-package lsp-ui
    :straight t
    :hook (lsp-mode . (lambda ()
                        (lsp-ui-mode)
                        (setq-local evil-lookup-func 'lsp-describe-thing-at-point))))

(use-package posframe
    :straight t)

(use-package dap-mode
    :straight t
    :hook (lsp-mode . dap-mode)
    (lsp-mode . dap-ui-mode))

(use-package flycheck
    :straight t
    :hook (lsp-mode . flycheck-mode)
    :general
    (local-leader-def
        :keymaps 'flycheck-mode-map
      "[" 'flycheck-list-errors
      ))

(use-package eglot
    :straight t
    :config
    (setq eglot-autoshutdown t))

(provide 'lsp-conf)
