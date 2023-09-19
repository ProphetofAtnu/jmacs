;; -*- lexical-binding: t; -*-

(use-package geiser
    :straight t
    :general
    (local-leader-def
        :keymaps '(scheme-mode-map)
      "e" 'geiser-eval-definition
      "E" 'geiser-eval-definition-and-go
      "b" 'geiser-eval-buffer
      "B" 'geiser-eval-buffer-and-go
      "r" 'geiser-eval-region
      "s" 'geiser-set-scheme
      "d" 'geiser-doc-module
      "," 'geiser-mode-switch-to-repl)
    :config
    (setq geiser-default-implementation 'guile))

(defconst geiser-gambit-minimum-version "4.9.3")

(use-package geiser-chez
    :straight t)

(use-package geiser-guile
    :straight t)

(use-package geiser-gambit
    :straight t)

(use-package geiser-chicken
    :straight t)

(use-package geiser-gauche
    :straight t)

(use-package geiser-racket
    :straight t)

(provide 'scheme-conf)
