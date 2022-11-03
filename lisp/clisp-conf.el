;;; -*- lexical-binding: t; -*-

(require 'use-package)

(require 'general)

(setq inferior-lisp-program "ros -Q run")

(use-package slime-company
  :straight t)

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
  (slime-setup '(slime-fancy slime-company))
  ;; (add-hook 'slime-completion-at-point-functions
  ;;                (cape-company-to-capf #'company-slime))
  ;; (remove-hook 'slime-completion-at-point-functions #'slime-c-p-c-completion-at-point)
  )


;; (use-package sly-macrostep
;;     :straight t
;;     :after (sly))

;; (use-package sly-quicklisp
;;     :straight t
;;     :after (sly))

;; (use-package sly-hello-world
;;     :straight t
;;     :after (sly))

;; (use-package sly-repl-ansi-color
;;     :straight t
;;     :after (sly))

;; (use-package sly-named-readtables
;;     :straight t
;;     :after (sly))

;; (use-package sly
;;     :straight t
;;     :hook (lisp-mode . sly-mode)
;;     :general
;;     (local-leader-def
;;         :keymaps '(lisp-mode-map)
;;       "," 'sly
;;       "q" 'sly-reset
;;       "Q" 'sly-quit-lisp
;;       "a" 'sly-apropos-all
;;       "x" 'sly-scratch
;;       "r" 'sly-pprint-eval-region
;;       "e" 'sly-eval-defun
;;       "E" 'sly-pprint-eval-last-expression
;;       "b" 'sly-eval-buffer
;;       "d" 'sly-cd
;;       "m" 'macrostep-mode
;;       "D" 'sly-pwd)
;;     :init
;;     (add-to-list 'popup-buffer-identifiers "\\*sly-description\\*")
;;     :config
;;     (setq sly-contribs '(sly-mrepl
;;                          sly-fancy
;;                          sly-tramp
;;                          sly-autodoc
;;                          sly-scratch
;;                          sly-stickers
;;                          sly-package-fu
;;                          sly-fancy-trace
;;                          sly-indentation
;;                          sly-trace-dialog
;;                          sly-fontifying-fu
;;                          sly-fancy-inspector))
;;     (sly-symbol-completion-mode +1)
;;     )

(provide 'clisp-conf)
