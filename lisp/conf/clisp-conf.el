;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)

(setq inferior-lisp-program "ros -Q run")

(defvar js/lisprc-path (expand-file-name "tools/cl-init.lisp" user-emacs-directory))

(straight-use-package
 `(sly-fix
   :type nil
   :local-repo ,(expand-file-name
		 "lisp/packages/sly-fix"
		 user-emacs-directory)
   :build t))

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
    )
  :config
  ;; (require 'sly-fix)
  ;; (with-eval-after-load "sly-completion"
  ;;   (fset 'sly--completion-function-wrapper
  ;; 	  #'js/sly--completion-function-wrapper))
  (setq sly-contribs
	'(sly-fancy
	  sly-mrepl
	  sly-scratch
	  sly-stickers))
  (add-hook 'sly-mode-hook 'company-mode)
  (defun js/disable-sly-completion ()
    (setq-local tab-always-indent t)
    (sly-symbol-completion-mode -1))

  (sly-setup)
  (add-hook 'sly-mode-hook 'js/disable-sly-completion 100)
  (add-hook 'sly-mrepl-mode-hook 'js/disable-sly-completion 100)
  )


;; (use-package slime-company
;;   :straight t
;;   :commands (company-slime)
;;   :config
;;   (setq slime-company-completion 'fuzzy))

;; (use-package slime
;;   :straight t
;;   :init
;;   (setq slime-contribs '(slime-asdf
;;                          slime-fancy
;;                          slime-indentation
;;                          slime-sbcl-exts
;;                          slime-scratch))
;;   ;; enable fuzzy matching in code buffer and SLIME REPL
;;   (setq slime-complete-symbol*-fancy t)
;;   (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
;;   ;; enable fuzzy matching in code buffer and SLIME REPL
;;   :config
;;   (slime-setup)
;;   (fset 'corfu-slime-capf (cape-company-to-capf 'company-slime))
;;   (defun js/slime-company-hook ()
;;     (make-local-variable 'completion-at-point-functions)
;;     (add-to-list 'completion-at-point-functions
;;                  #'corfu-slime-capf))
;;   (add-hook 'slime-repl-mode-hook #'js/slime-company-hook)
;;   (add-hook 'slime-mode-hook #'js/slime-company-hook))

;; (use-package macrostep
;;   :straight t
;;   :general
;;   (local-leader-def
;;     :keymaps '(lisp-mode-map)
;;     "m" 'macrostep-mode))

(provide 'clisp-conf)
