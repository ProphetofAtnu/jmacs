;;; -*- lexical-binding: t; -*-

(use-package elixir-mode
    :straight t
    :defer t
    :config
    (setq lsp-elixir-suggest-specs nil)
    (add-to-list 'exec-path (expand-file-name "~/.install/elixir-ls/release")))

(use-package lsp-mode
    :straight t
    :defer t
    :hook
    (elixir-mode . lsp-deferred))

(use-package mix
    :straight t
    :general
    (local-leader-def
        :keymaps 'elixir-mode-map
      "c" 'mix-compile
      "t" 'mix-test-current-test
      "T" 'mix-test
      "x" 'mix-execute-task))

(provide 'elixir-conf)
