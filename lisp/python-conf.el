;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)

;; (use-package elpy
;;     :straight t
;;     :general
;;     (local-leader-def
;;         :keymaps 'elpy-mode-map
;;       "d" 'elpy-shell-send-statement)
;;     :init
;;     (elpy-enable)
;;     (delq 'elpy-module-company elpy-modules)
;;     (delq 'elpy-module-flymake elpy-modules)
;;     (add-to-list 'popup-buffer-identifiers "\\*Python Doc\\*")
;;     :config
;;     (add-hook 'elpy-mode-hook
;;               #'(lambda ()
;;                   (setq-local evil-lookup-func #'elpy-doc)
;;                   (add-hook 'completion-at-point-functions
;;                             (cape-company-to-capf 'elpy-company-backend) 0 t))))

;; (use-package anaconda-mode
;;     :straight t
;;     :hook (python-mode . anaconda-mode))

(use-package lsp-pyright
  :straight t)

(use-package lsp-mode
    :straight t
    :hook (python-mode . lsp-deferred))

;; (use-package eglot
;;     :straight t
;;     :hook (python-mode . eglot-ensure)
;;     :config
;;     (push '(python-mode . ("pyright-langserver" "--stdio"))
;;           eglot-server-programs))

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
    ;; (add-hook
    ;;  'jupyter-repl-mode-hook 'company-mode)
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
