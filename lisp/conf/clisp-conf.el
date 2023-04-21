;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)

(setq inferior-lisp-program "ros -Q run")

(defvar js/lisprc-path (expand-file-name "tools/cl-init.lisp" user-emacs-directory))


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
    ))


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
;;   :config
;;   (slime-setup))

;; (use-package macrostep
;;   :straight t
;;   :general
;;   (local-leader-def
;;     :keymaps '(lisp-mode-map)
;;     "m" 'macrostep-mode))

(provide 'clisp-conf)
