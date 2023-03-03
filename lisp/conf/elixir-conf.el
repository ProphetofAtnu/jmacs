;;; -*- lexical-binding: t; -*-

(require 'mode-local)

(use-package elixir-mode
 :straight t
 :defer t)

(use-package elixir-ts-mode
 :straight t
 :defer t)


(use-package lsp-mode
    :straight t
    :defer t
    :hook
    ((elixir-mode elixir-ts-mode) . lsp-deferred)
    :config
    (setq lsp-elixir-suggest-specs nil))

(use-package mix
    :straight t
    :general
    (local-leader-def
        :keymaps '(elixir-mode-map elixir-ts-mode-map)
      "c" 'mix-compile
      "t" 'mix-test-current-test
      "T" 'mix-test
      "x" 'mix-execute-task))

(use-package smartparens
  :straight t
  :config
  (sp-with-modes '(elixir-mode elixir-ts-mode)
    (sp-local-pair "do" "end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :skip-match 'sp-elixir-skip-def-p
                   :unless '(sp-in-comment-p sp-in-string-p)
		   :post-handlers '("| "))
    ))



(setq-mode-local elixir-mode
                 tab-width 2)

(setq-mode-local elixir-ts-mode
                 tab-width 2)

(provide 'elixir-conf)
