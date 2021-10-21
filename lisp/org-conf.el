;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)


(require 'use-package)
(require 'general)

(use-package org
  :defer t
  :commands (org-agenda
             org-capture)
  :init
  (defvar org-directory "~/org")
  :general
  (local-leader-def
      :keymaps 'org-mode-map
    "," 'consult-org-heading
    "|" 'org-table-create-or-convert-from-region
    "y" 'org-store-link
    "i" 'org-insert-structure-template
    "p" 'org-insert-last-stored-link)
  :config
  (add-hook 'org-mode-hook 'company-mode)
  (add-hook 'org-mode-hook 'org-display-inline-images))

(use-package evil-org
    :straight t
    :hook (org-mode . evil-org-mode))
  
(use-package deft
  :straight t
  :commands (deft)
  :general
  (global-leader-def
      :keymaps 'override
      "n" 'deft
      "N" 'deft-new-file)
  :config
  (add-to-list 'evil-emacs-state-modes 'deft-mode)
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t)
  (setq deft-default-extension "org")
  (setq deft-use-filter-string-for-filename t)
  (setq deft-file-naming-rules
        '((nospace . "-")
          (case-fn . downcase)))
  (setq deft-directory
        (expand-file-name "deft" org-directory)))
  


(provide 'org-conf)
