;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)

(use-package python
    :defer t
    :config
    (setq python-indent-guess-indent-offset-verbose nil))

;; (use-package company-jedi
;;     :straight t
;;     :general
;;     (jedi-mode-map
;;      :states '(normal motion)
;;      "K" 'jedi:show-doc)
;;     :hook (python-mode . jedi:setup)
;;     :init
;;     (defun cape-jedi-setup ()
;;       (let ((jedi-capf (cape-company-to-capf 'company-jedi)))
;;         (add-hook 'completion-at-point-functions jedi-capf 0 t)))
;;     (add-hook 'jedi-mode-hook 'cape-jedi-setup))

;; (use-package lsp-pyright
;;     :straight t)

;; (use-package lsp-mode
;;     :straight t
;;     :hook (python-mode . lsp-deferred))

(use-package eglot
    :straight t
    :hook (python-mode . eglot-ensure)
    :config
    (push '(python-mode . ("pyright-langserver" "--stdio"))
          eglot-server-programs))

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

(use-package jupyter
    :straight t
    :config
    (require 'ob-jupyter)
    (setq org-babel-default-header-args:jupyter-python
          '((:async . "yes")
            (:session . "py")
            (:kernel . "python3")))
    (setq jupyter-org-adjust-image-size nil)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (julia . t)
       (python . t)
       (jupyter . t)))
    (add-hook
     'jupyter-repl-mode-hook 'company-mode)
    (local-leader-def
        :definer 'minor-mode
      :keymaps '(jupyter-org-interaction-mode)
      "b" 'jupyter-org-insert-src-block)
    (jupyter-org-define-key (kbd "C-M-RET") 'jupyter-eval-defun)
    (jupyter-org-define-key (kbd "C-M-<return>") 'jupyter-eval-defun))

(use-package org
    :defer t
    :config)


(provide 'python-conf)
