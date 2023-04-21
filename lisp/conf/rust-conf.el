;;; -*- lexical-binding: t; -*-

;; (use-package rust-mode
;;   :straight t)

(use-package rustic
  :straight t
  :demand t
  :general
  (local-leader-def
    :keymaps '(rustic-mode-map)
    "," 'rustic-popup
    "x" 'rustic-cargo-run
    "c" 'rustic-cargo-build
    "=" 'rustic-cargo-fmt)
  :config
  (add-to-list 'evil-emacs-state-modes
               'rustic-popup-mode))

(use-package lsp-mode
  :straight t
  :hook (rustic-mode . lsp-deferred))

(provide 'rust-conf)
