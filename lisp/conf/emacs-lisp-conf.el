;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)
(require 'core/formatting)

(use-package helpful
  :straight t
  :commands (helpful-at-point))

(use-package macrostep
  :straight t
  :commands (macrostep-mode))

;; (defun yas-use-elisp-snippets ()
;;   (setq-local yas--extra-modes '(emacs-lisp-mode)))

(use-package elisp-def
  :delight
  :straight t
  :hook ((emacs-lisp-mode . elisp-def-mode)
         (lisp-interaction-mode . elisp-def-mode)
         (ielm-mode . elisp-def-mode)))

(use-package lisp-extra-font-lock
  :straight t
  :hook (emacs-lisp-mode . lisp-extra-font-lock-mode))

(use-package inspector
  :straight t
  :config
  (define-prefix-map elisp-inspect
		     "e" 'inspect-expression
		     "s" 'inspect-last-sexp
		     "o" 'inspect-object))

(use-package eros
  :straight t
  :hook (emacs-lisp-mode . eros-mode))

(use-package ielm
  :commands (ielm)
  :config
  (setq ielm-dynamic-return nil)
  ;; (add-hook 'ielm-mode-hook #'yas-use-elisp-snippets)
  (add-hook
   'ielm-mode-hook
   #'(lambda ()
       (setq-local
        evil-lookup-func
        #'helpful-at-point))))

(defun elisp-add-directory-to-load-path (directory)
  (interactive "D")
  (add-to-list 'load-path directory))

(defun elisp-auto-insert-lex ()
  (interactive)
  (when (and
         (eq major-mode 'emacs-lisp-mode)
         (buffer-file-name)
         (not (save-excursion
                (goto-char (point-min))
                (looking-at ".*lexical-binding: t"))))
    (save-excursion
      (add-file-local-variable-prop-line 'lexical-binding t))))

(defun emacs-lisp-setup-hook ()
  ;; (setq-local lisp-indent-function #'common-lisp-indent-function)
  (setq-local evil-lookup-func #'helpful-at-point)
  (setq-local company-backends
	      '(company-capf
		(company-dabbrev-code company-gtags company-etags company-keywords)
		company-dabbrev))
  (setq-local completion-at-point-functions
	      '(elisp-completion-at-point
		cape-dabbrev t)))

;; (when corfu-global-mode
;;   (setq-local completion-at-point-functions '(
;;               elisp-completion-at-point
;;               cape-symbol
;;               cape-dabbrev
;;               cape-keyword
;;               cape-file t)))
;; (elisp-auto-insert-lex)


(use-package elisp-mode
  :commands (elisp-mode)
  :config
  (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-setup-hook))

(local-leader-def
  :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
  "i" (mount-prefix-map elisp-inspect "Inspector")
  "l" 'elisp-auto-insert-lex
  "e" 'eval-last-sexp
  "d" 'eval-defun
  "r" 'eval-region
  "b" 'eval-buffer
  "p" 'indent-pp-sexp
  "=" 'parinfer-auto-fix
  "L" 'elisp-add-directory-to-load-path
  "m" 'macrostep-mode)

(use-package emr
  :straight t
  :general
  (local-leader-def
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "," 'emr-show-refactor-menu
    "l" 'emr-el-extract-to-let))

(use-package edebug
  :init 
  (defun edebug-wc-top-level ()
    (interactive)
    (which-key--show-keymap "edebug" edebug-mode-map))
  (general-defs
    :keymaps 'edebug-mode-map
    "?" 'edebug-wc-top-level))

(provide 'emacs-lisp-conf)
