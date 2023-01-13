;; -*- lexical-binding: t; -*-

(use-package tree-sitter
  :straight t
  :hook (emacs-startup . global-tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-indent
  :straight t)

(use-package tree-sitter-langs
  :straight t)

(use-package treemacs
  :straight t
  :commands (treemacs))

(provide 'global/global-tree-sitter)
