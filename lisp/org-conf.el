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

  :straight t)
  
(use-package deft
  :straight t
  :commands (deft)
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
