;;; -*- lexical-binding: t; -*-

(use-package emmet-mode
    :straight t
    :hook ((html-mode css-mode) . emmet-mode)
    :general
    (local-leader-def
        :keymaps 'emmet-mode-keymap))
  
(provide 'sgml-conf)
