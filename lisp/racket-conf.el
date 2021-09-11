;;; -*- lexical-binding: t; -*-

(use-package racket-mode
    :straight t
    :mode ((("\\.rktl\\'" . racket-mode))
           ("\\.rktd\\'" . racket-mode)
           ("\\.rkt\\'" . racket-mode)))

(provide 'racket-conf)
