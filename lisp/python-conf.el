;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)

(use-package python-mode
  :straight t
  :defer t)

(use-package company-anaconda
  :straight t
  :commands (company-anaconda))

(use-package anaconda-mode
  :straight t
  :after (python-mode)
  :hook (python-mode . anaconda-mode)
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package python-x
  :straight t
  :after (python-mode)
  :config
  (python-x-setup))



(provide 'python-conf)
