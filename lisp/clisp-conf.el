;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)

(setq inferior-lisp-program "ros -Q run")

(defvar js/lisprc-path (expand-file-name "tools/cl-init.lisp" user-emacs-directory))

;; (use-package slime-company
;;   :straight t
;;   :defer t
;;   :commands (company-slime))

;; (defun js/slime-user-init ()
;;   (if (file-exists-p js/lisprc-path)
;;       (slime-load-file js/lisprc-path)))

;; (use-package slime
;;   :straight t
;;   ;; :hook (common-lisp-mode . slime)
;;   ;; :general
;;   ;; (local-leader-def
;;   ;;   :keymaps '(lisp-mode-map)
;;   ;;   "e" 'slime-eval-defun
;;   ;;   "E" 'slime-eval-print-last-expression
;;   ;;   "c" 'slime-cd
;;   ;;   "," 'slime-repl)
;;   :config
;;   (defun js/slime-company-hook ()
;;     (add-hook 'company-backends #'company-slime nil t)
;;     (company-mode +1))
;;   (add-hook 'slime-mode-hook #'js/slime-company-hook)
;;   (add-hook 'slime-repl-mode-hook #'js/slime-company-hook)
;;   (setq inferior-lisp-program "ros -Q run")
;;   (add-hook 'slime-connected-hook #'js/slime-user-init)
;;   (slime-setup '(slime-fancy
;;                  slime-company
;;                  slime-scratch
;;                  slime-repl
;;                  slime-asdf
;;                  slime-macrostep
;;                  slime-quicklisp
;;                  slime-sbcl-exts))
;;   ;; (add-hook 'slime-completion-at-point-functions
;;   ;;                (cape-company-to-capf #'company-slime))
;;   ;; (remove-hook 'slime-completion-at-point-functions #'slime-c-p-c-completion-at-point)
;;   )

(use-package sly
  :straight t
  :general
  (local-leader-def
    :keymaps '(lisp-mode-map)
    "," 'sly
    "e" 'sly-pprint-eval-last-expression
    "E" 'sly-eval-print-last-expression
    "c" 'sly-cd
    "s" 'sly-mrepl-shortcut
    "S" 'sly-mrepl-sync
    "q" 'sly-restart-inferior-lisp
    "b" 'sly-eval-buffer
    "d" 'sly-eval-defun
    "r" 'sly-eval-region
    "v" 'sly-edit-value
    "z" 'sly-import-symbol-at-point
    "x" 'sly-export-symbol-at-point
    "/" 'sly-documentation
    "?" 'sly-documentation-lookup
    "i" '(:ignore t :wk "Inspect")
    "i i" 'sly-inspect
    "i d" 'sly-inspect-definition
    "t" '(:ignore t :wk "Stickers")
    "t t" 'sly-stickers-dwim
    "t f" 'sly-stickers-fetch
    "t F" 'sly-stickers-forget
    "t B" 'sly-stickers-clear-buffer-stickers
    "t D" 'sly-stickers-clear-defun-stickers
    "t R" 'sly-stickers-clear-region-stickers
    "u" '(:ignore t :wk "Unintern")
    "u u" 'sly-unintern-symbol
    "u f" 'sly-undefine-function
    "u p" 'sly-delete-package
    ;; end Stickers
    )
  :config
  (sly-setup
   '(sly-fancy sly-mrepl sly-scratch))
  (defun js/sly-company-hook ()
    (sly-symbol-completion-mode -1)
    (setq-local
     company-frontends
     '(company-pseudo-tooltip-unless-just-one-frontend
       company-preview-if-just-one-frontend))
    (company-mode 1))
  (add-hook 'sly-mode-hook #'js/sly-company-hook)
  (add-hook 'sly-mrepl-mode-hook #'js/sly-company-hook))

;; (use-package macrostep
;;   :straight t
;;   :general
;;   (local-leader-def
;;     :keymaps '(lisp-mode-map)
;;     "m" 'macrostep-mode))

(provide 'clisp-conf)
