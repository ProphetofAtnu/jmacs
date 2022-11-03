;;; -*- lexical-binding: t; -*-

(require 'mode-local)

(use-package elixir-mode
 :straight t
 :defer t
 :config
 (setq lsp-elixir-suggest-specs nil)
 (setq lsp-elixir-ls-server-dir (expand-file-name "~/.install/elixir-ls/release")))

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

(setq-mode-local elixir-mode
                 tab-width 2)

(provide 'elixir-conf)
