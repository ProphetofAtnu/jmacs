;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)

(use-package python
  :defer t)


(use-package lsp-mode
    :straight t
    :defer t
    :hook (python-mode . lsp-deferred))

;; (use-package company-anaconda
;;     :straight t
;;     :commands (company-anaconda))


;; (use-package anaconda-mode
;;   :straight t
;;   :after (python)
;;   :hook (python-mode . anaconda-mode)
;;   :config
;;   (add-to-list 'company-backends 'company-anaconda))

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
