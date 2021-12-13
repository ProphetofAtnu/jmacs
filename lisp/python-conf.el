;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)

(use-package python
  :defer t)


;; (use-package lsp-mode
;;     :straight t
;;     :defer t
;;     :hook (python-mode . lsp-deferred))

(use-package elpy
    :straight t
    :hook (python-mode . elpy-enable)
    :config
    (setq elpy-rpc-python-command "python3")
    (setq elpy-get-info-from-shell t))

(use-package python-black
    :straight t
    :general
    (local-leader-def
        :keymaps '(python-mode-map)
      "=" 'python-black-buffer))          

(use-package python-x
  :straight t
  :after (python)
  :general
  (local-leader-def
      :keymaps '(python-mode-map)
    "e" 'python-shell-send-dwim)
  :config
  (python-x-setup))



(provide 'python-conf)
