;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)

;; (straight-use-package `(pymacs
;;                         :type git
;;                         :host github
;;                         :repo "pymacs2/Pymacs"
;;                         :pre-build ,(cl-letf
;;                                         (((symbol-function
;;                                            #'el-get-package-directory)
;;                                           (lambda (package)
;;                                             (straight--repos-dir
;;                                              (format "%S" package))))
;;                                          (el-get-install-info
;;                                           (straight--el-get-install-info))
;;                                          (el-get-emacs
;;                                           (straight--el-get-emacs))
;;                                          (el-get-dir
;;                                           (straight--repos-dir)))
;;                                       `(("python3"
;;                                          "setup.py"
;;                                          "--quiet"
;;                                          "build")))
;;                         :files (:defaults)))

;; (use-package pymacs
;;     :straight t
;;     :config
;;     (setq pymacs-python-command "python3"))


;; (use-package python-mode
;;     :straight t
;;     :init
;;     (add-to-list 'load-path
;;                  (expand-file-name "completion" (straight--repos-dir "python-mode")))
;;     :config
;;     (add-to-list
;;      'pymacs-load-path
;;      (expand-file-name
;;       "completion"
;;       (straight--repos-dir
;;        "python-mode")))
;;     (add-hook
;;      'python-mode-hook
;;      (lambda ()
;;        (require 'pycomplete)
;;        (add-hook 'completion-at-point-functions #'py-complete-completion-at-point nil t)))
;;     (setq py-python-command "python3"
;;           py-install-directory
;;           (straight--repos-dir "python-mode")
;;           py-load-pymacs-p nil
;;           py-indent-no-completion-p t
;;           py-complete-function 'py-complete))

;; (use-package python
;;     :defer t
;;     :config
;;     (setq python-indent-guess-indent-offset-verbose nil))

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
