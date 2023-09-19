;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)
(require 'evil)

(use-package lispy
  :straight t
  :hook ((emacs-lisp-mode
          lisp-mode
          clojure-mode
          common-lisp-mode
	  racket-mode
          scheme-mode)
         . lispy-mode)
  :init
  :config
  (keymap-set lispy-mode-map-lispy "DEL" 'lispy-delete-backward-or-splice-or-slurp)
  (keymap-set lispy-mode-map-lispy "TAB" nil)
  (keymap-set lispy-mode-map "TAB" nil)
  (general-defs
    :keymaps '(lispy-mode-map)
    :states '(normal)
    "M-[" 'lispy-wrap-brackets
    "M-]" 'lispy-wrap-brackets)
  (setq lispy-completion-method 'default
        lispy-close-quotes-at-end-p t
        lispy-colon-p nil)
  (add-to-list 'lispy-compat 'cider)
  (add-to-list 'lispy-compat 'macrostep))

(use-package lispyville
  :straight t
  :hook (lispy-mode . lispyville-mode))

(use-package parinfer
  :straight t
  :commands (parinfer-auto-fix))

(use-package rainbow-delimiters
  :straight t
  :hook ((emacs-lisp-mode
          lisp-mode
          clojure-mode
          lisp-interaction-mode
          common-lisp-mode
          scheme-mode)
         . rainbow-delimiters-mode))

(use-package evil-smartparens
  :straight t
  ;; :hook ((emacs-lisp-mode
  ;;         lisp-mode
  ;;         lisp-interaction-mode
  ;;         clojure-mode
  ;;         common-lisp-mode
  ;;         scheme-mode)
  ;;        . evil-smartparens-mode)
  )


(use-package evil-cleverparens
  :straight t
  ;; :hook ((emacs-lisp-mode
  ;;         lisp-mode
  ;;         lisp-interaction-mode
  ;;         clojure-mode
  ;;         common-lisp-mode
  ;;         scheme-mode)
  ;;        . evil-cleverparens-mode)
  :general
  (:keymaps '(evil-cleverparens-mode-map)
            :states '(normal visual motion)
            "(" 'evil-cp-backward-sexp
            ")" 'evil-cp-forward-sexp)
  :init
  (setq evil-cleverparens-use-additional-movement-keys nil
        ;; evil-cleverparens-swap-move-by-word-and-symbol t
        evil-cleverparens-use-s-and-S nil
        evil-move-beyond-eol t))

(defun enable-evil-cp-and-sp ()
  (evil-cleverparens-mode +1)
  (evil-smartparens-mode +1))

(defun disable-evil-cp-and-sp ()
  (evil-cleverparens-mode -1)
  (evil-smartparens-mode -1))


(require 'evil-cleverparens-text-objects)

(provide 'sexp-conf)

