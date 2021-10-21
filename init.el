;;; -*- lexical-binding: t; -*-

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)

(straight-use-package 'use-package)
(require 'use-package)

(setq use-package-compute-statistics t)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(setq create-lockfiles nil)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "platform" user-emacs-directory))

(setq visible-bell nil
      ring-bell-function #'ignore
      inhibit-startup-screen t
      comint-prompt-read-only t
      sentence-end-double-space nil
      gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(defvar evil-disable-insert-state-bindings t)

(use-package better-defaults
    :straight t)

(use-package no-littering
    :straight t)

(use-package delight
    :straight t)

;; Evil

(use-package evil 
    :straight t
    :delight
    :init 
    (setq evil-want-C-u-scroll t
          evil-undo-system 'undo-redo
          evil-ex-search-vim-style-regexp t
          evil-search-module 'evil-search)

    (evil-mode))

(use-package evil-collection
    :straight t
    :init 
    (evil-collection-init))

(use-package evil-goggles
    :straight t
    :delight
    :config
    (evil-goggles-use-diff-faces)
    (evil-goggles-mode))

(use-package evil-surround
    :straight t
    :commands
    (evil-surround-edit
     evil-Surround-edit
     evil-surround-region
     evil-Surround-region)
    :init
    (evil-define-key 'operator global-map "s" 'evil-surround-edit)
    (evil-define-key 'operator global-map "S" 'evil-surround-edit)
    (evil-define-key 'visual global-map "s" 'evil-surround-region)
    (evil-define-key 'visual global-map "S" 'evil-Surround-region))

(use-package evil-matchit
    :straight t
    :hook (emacs-startup . global-evil-matchit-mode))

(use-package evil-commentary
    :straight t
    :bind
    (:map evil-normal-state-map)
    ("gc" . evil-commentary))

(use-package evil-better-visual-line
    :straight t
    :defer 5
    :config
    (add-hook 'visual-line-mode-hook #'evil-better-visual-line-on))

(use-package avy
    :straight t)

(use-package which-key
    :straight t
    :delight
    :hook (emacs-startup . which-key-mode))

(use-package general
    :straight t
    :hook (emacs-startup . general-override-mode)
    :init
    (require 'core/bindings))

(use-package evil-avy
    :straight t
    :general
    (:states '(normal)
             "s" 'evil-avy-goto-char-2
             "g l" 'evil-avy-goto-line))

(use-package company
    :straight t
    :hook ((prog-mode comint-mode) . company-mode)
    :general
    (:states '(insert emacs)
             "C-TAB" 'company-complete
             "C-<tab>" 'company-complete)
    :config
    (setq company-minimum-prefix-length 1))

(use-package restart-emacs
    :straight t
    :commands (restart-emacs))

(use-package recentf
    :hook (emacs-startup . recentf-mode))

(use-package magit
    :straight t
    :commands (magit)
    :general
    (:keymaps 'prefix-project-map
              "m" 'magit))

(use-package markdown-mode
    :straight t
    :mode "\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'")

;; Themeing
(use-package bliss-theme
    :straight t
    :init
    (load-theme 'bliss t)
    :config
    (set-face-background 'highlight "#3d3d3d"))
    

(use-package mood-line
    :straight t
    :init
    (mood-line-mode))

(use-package exec-path-from-shell
    :straight t
    :hook (emacs-startup . exec-path-from-shell-initialize))

(set-face-attribute 'default t :font "Fira Code-12")
(set-frame-font "Fira Code-12" nil t)

(add-hook 'prog-mode-hook 'prettify-symbols-mode)

(use-package helpful
    :straight t
    :commands (helpful-symbol
               helpful-key))

(use-package ace-window
    :straight t
    :commands (ace-window
               ace-swap-window))

(use-package yasnippet
    :straight t
    :hook (emacs-startup . yas-global-mode)
    :config
    (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))

(use-package tab-bar
    :init
  (setq tab-bar-show nil))

(use-package insert-time
    :straight t
    :general
    (:keymaps 'prefix-insert-map
              "t" 'insert-date-time
              "T" 'insert-time))

(use-package all-the-icons
    :straight t)

(use-package hl-spotlight
    :straight t)

(use-package hl-line+
    :straight t)

(use-package treemacs
    :straight t
    :commands (treemacs)
    :general
    (global-leader-def
        :keymaps 'override
        "t" 'treemacs))

(use-package projectile
    :straight t
    :hook (emacs-startup . projectile-mode)
    :general
    (:keymaps 'prefix-project-map
              "s" 'projectile-switch-project
              "a" 'projectile-add-known-project
              "R" 'projectile-remove-current-project-from-known-projects
              "t" 'projectile-regenerate-tags
              "g" 'projectile-ripgrep
              "!" 'projectile-cleanup-known-projects
              "x" 'projectile-run-project
              "X" 'projectile-compile-project
              "f" 'projectile-find-file-dwim
              "c" 'projectile-commander
              "C" 'projectile-kill-buffers))    
              
              

(cond
  ((eq system-type 'darwin) (require 'macos)))

(require 'sexp-conf)
(require 'emacs-lisp-conf)
(require 'selectrum-conf)
(require 'clojure-conf)
(require 'clisp-conf)
(require 'shell-conf)
(require 'org-conf)
(require 'python-conf)
(require 'docker-conf)
(require 'lsp-conf)
(require 'racket-conf)
(require 'web-conf)

(load custom-file)
(put 'dired-find-alternate-file 'disabled nil)
