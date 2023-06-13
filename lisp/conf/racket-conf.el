;;; -*- lexical-binding: t; -*-

(use-package racket-mode
    :straight t
    :general
    (local-leader-def
      :keymaps '(racket-mode-map)
      "e" 'racket-eval-last-sexp
      "s" 'racket-describe-search
      "," 'racket-repl
      )
    :mode (("\\.rktl\\'" . racket-mode)
	   ("\\.rktd\\'" . racket-mode)
	   ("\\.rkt\\'" . racket-mode))
    :config
    (setq-mode-local racket-mode evil-lookup-func #'racket-xp-describe)
    (add-hook 'racket-mode-hook 'racket-xp-mode))

(provide 'racket-conf)
