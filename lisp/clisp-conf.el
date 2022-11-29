;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)

(setq inferior-lisp-program "ros -Q run")

(use-package slime-company
  :straight t
  :defer t
  :commands (company-slime))


(defvar js/lisprc-path (expand-file-name "tools/cl-init.lisp" user-emacs-directory))

(defun js/slime-user-init ()
  (if (file-exists-p js/lisprc-path)
      (slime-load-file js/lisprc-path)))

(use-package slime
  :straight t
  :hook (common-lisp-mode . slime)
  :general
  (local-leader-def
    :keymaps '(lisp-mode-map)
    "e" 'slime-eval-defun
    "E" 'slime-eval-print-last-expression
    "c" 'slime-cd
    "," 'slime-repl)
  :config
  (defun js/slime-company-hook ()
    (add-hook 'company-backends #'company-slime nil t)
    (company-mode +1))
  (add-hook 'slime-mode-hook #'js/slime-company-hook)
  (add-hook 'slime-repl-mode-hook #'js/slime-company-hook)
  (setq inferior-lisp-program "ros -Q run")
  (add-hook 'slime-connected-hook #'js/slime-user-init)
  (slime-setup '(slime-fancy
                 slime-company
                 slime-scratch
                 slime-repl
                 slime-asdf
                 slime-macrostep
                 slime-quicklisp
                 slime-sbcl-exts))
  ;; (add-hook 'slime-completion-at-point-functions
  ;;                (cape-company-to-capf #'company-slime))
  ;; (remove-hook 'slime-completion-at-point-functions #'slime-c-p-c-completion-at-point)
  )

(provide 'clisp-conf)
