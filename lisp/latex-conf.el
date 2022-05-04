;; -*- lexical-binding: t; -*-

(straight-use-package 'auctex)

(define-prefix-map latex-preview)

(use-package latex
    :general
  (local-leader-def
      :keymaps '(LaTeX-mode-map latex-mode-map)
    "," 'TeX-command-master
    "e" 'LaTeX-environment
    "s" 'LaTeX-section
    "m" 'LaTeX-math-mode
    "p" (mount-prefix-map latex-preview "Preview"))
  (general-defs
      'prefix-latex-preview-map
      "b" 'preview-buffer
      "d" 'preview-document
      "s" 'preview-section
      "p" 'preview-at-point))


(provide 'latex-conf)
