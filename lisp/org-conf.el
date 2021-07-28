;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)

(use-package org
  :defer t
  :commands (org-agenda
             org-capture)
  :straight t)
  
(use-package deft
  :straight t
  :commands (deft)
  :config
  (setq deft-directory
        (expand-file-name "deft" org-directory)))
  


(provide 'org-conf)
