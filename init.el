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

(defvar evil-want-keybinding nil)

(straight-use-package 'dired-hacks)
(straight-use-package 'general)
(straight-use-package 'hydra)
(straight-use-package 'm-buffer)
(straight-use-package 'treepy)
(straight-use-package 'tablist)
(straight-use-package 'ts)
(straight-use-package 'svg-lib)
(straight-use-package 'anaphora)
(straight-use-package 'datetime)
(straight-use-package 'deferred)
(straight-use-package 'evil-mc)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(setq create-lockfiles nil)

(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "util" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "util/tools" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "platform" user-emacs-directory))

(setq visible-bell nil
      ring-bell-function #'ignore
      inhibit-startup-screen t
      comint-prompt-read-only t
      sentence-end-double-space nil
      gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

(byte-recompile-directory (expand-file-name "util" user-emacs-directory) 0)
(byte-recompile-directory (expand-file-name "lisp/core" user-emacs-directory) 0)

(defvar js/should-load-and-compile-directories
  (list
   (expand-file-name "util" user-emacs-directory)
   (expand-file-name "lisp/core" user-emacs-directory)))

(setq evil-want-integration t)
(defvar evil-disable-insert-state-bindings t)

;; (add-to-list 'load-path "/usr/local/lib/erlang/lib/tools-3.5.1/emacs")

(use-package better-defaults
    :straight t)

(use-package no-littering
    :straight t)

(use-package dash
    :straight t)

(use-package delight
    :straight t)

(use-package fn
    :straight t)

(require 'core/utility)
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
    :config
    (delq 'lispy evil-collection-mode-list)
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

(use-package hydra
    :straight t)

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

(use-package which-key-posframe
    :straight t
    :delight
    :hook (which-key-mode . which-key-posframe-mode)
    :config
    (setq which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-center))


(use-package general
    :straight t
    :hook (emacs-startup . general-override-mode)
    :init
    (require 'core/bindings))

(use-package corfu
    :straight t
    :bind
    (:map corfu-map
          ("TAB" . corfu-next)
          ([tab] . corfu-next)
          ("S-TAB" . corfu-previous)
          ([backtab] . corfu-previous))
    :init
    (setq corfu-auto t
          corfu-auto-delay 0.1
          corfu-auto-prefix 2)
    (setq tab-always-indent 'complete)
    (defun corfu-enable-in-minibuffer ()
      "Enable Corfu in the minibuffer if `completion-at-point' is bound."
      (when (where-is-internal #'completion-at-point (list (current-local-map)))
        ;; (setq-local corfu-auto nil) Enable/disable auto completion
        (corfu-mode 1)))
    (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
    (add-hook 'eshell-mode-hook
              (lambda ()
                (setq-local corfu-auto nil)
                (corfu-mode)))
    (global-corfu-mode))

(use-package cape
    :straight t
    :config ;; Silence the pcomplete capf, no errors or messages!
    (advice-add
     'pcomplete-completions-at-point
     :around #'cape-wrap-silent)
    ;; Ensure that pcomplete does not write to the buffer
    ;; and behaves as a pure `completion-at-point-function'.
    (advice-add
     'pcomplete-completions-at-point
     :around #'cape-wrap-purify)
    :init
    (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
    (add-to-list 'completion-at-point-functions #'cape-keyword))


(use-package evil-mc
    :straight t
    :hook (emacs-startup . global-evil-mc-mode)
    :init
    (require 'core/hydras)
    :general
    (:keymaps '(evil-mc-key-map)
              :states '(normal)
              "z ." 'evil-mc-make-cursor-here 
              "z <" 'evil-mc-pause-cursors
              "z >" 'evil-mc-resume-cursors
              "z /" 'evil-mc-undo-all-cursors
              )
    :config
    (general-def
        :keymaps '(evil-mc-key-map)
      :states '(normal)
      "g.." 'evil-mc-hydra/body)
    (evil-mc-define-vars)
    (add-to-list 'evil-mc-incompatible-minor-modes 'lispy)
    ) 


(use-package evil-avy
    :straight t
    :general
    (:states '(normal)
             "s" 'evil-avy-goto-char-2
             "g l" 'evil-avy-goto-line))


(use-package company
    :straight t
    :general
    (:keymaps '(company-mode-map)
              :states '(insert emacs)
              "C-TAB" 'company-complete
              "C-<tab>" 'company-complete)
    :init
    ;; (add-hook 'emacs-startup-hook
    ;;           'global-company-mode)
    :config
    (setq company-minimum-prefix-length 1))

;; (use-package company-statistics 
;;     :straight t
;;     :hook (company-mode . company-statistics-mode))

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
;; (use-package bliss-theme
;;     :straight t
;;     :init
;;     (load-theme 'bliss t)
;;     :config
;;     (set-face-background 'highlight "#3d3d3d")
;;     (set-face-background 'lazy-highlight "#333333"))


 ;; '(mode-line-inactive
 ;;   ((((class color) (min-colors 88)) (:background "#111111" :foreground "#000000"))
 ;;    (t (:weight light :box nil :background "#202339" :foreground "#000000" :inherit (mode-line)))))
 
(use-package cherry-blossom-theme
    :straight t)
    ;; (load-theme 'cherry-blossom t)
    ;; (set-face-foreground 'mode-line-inactive "#5F5A60"))

(use-package modus-themes
    :straight t
    :config 
    (load-theme 'modus-vivendi t))

(use-package popup
    :straight t)

(use-package zoxide
    :straight t
    :general
    (prefix-file-map
     "z" 'zoxide-find-file
     "r" 'zoxide-travel
     "c" 'zoxide-cd))

(use-package doom-modeline
    :straight t
    :init
    (doom-modeline-mode)
    :config
    (setq doom-modeline-height 0)
    (setq inhibit-compacting-font-caches t))


(use-package exec-path-from-shell
    :straight t
    :hook (emacs-startup . exec-path-from-shell-initialize))

(set-face-attribute 'default t :font "Fira Code-13")
(set-frame-font "Fira Code-13" nil t)

(add-hook 'prog-mode-hook 'prettify-symbols-mode)

(use-package helpful
    :straight t
    :init
    (defvar read-symbol-positions-list nil)
    :commands (helpful-symbol
               helpful-key))

(use-package ace-window
    :straight t
    :commands (ace-window
               ace-swap-window)
    :config
    (setq aw-keys
          (cl-loop for k across "asdfjkl;"
                collect k)))

(use-package switch-window
    :straight t
    :config
  (setq switch-window-shortcut-style 'qwerty
        switch-window-multiple-frames t))

(use-package dired-filter)
(use-package dired-narrow)
(use-package dired-subtree)

(use-package tempel
    :straight t
    :bind (("M-+" . tempel-complete)
           ;; Alternative tempel-expand
           ("M-*" . tempel-insert)))

(use-package yasnippet
    :straight t
    :hook (emacs-startup . yas-global-mode)
    :config
    (general-def
        'yas-minor-mode-map
        "SPC"
      yas-maybe-expand)
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

(use-package tree-sitter
    :straight t
    :hook (emacs-startup . global-tree-sitter-mode))

(use-package tree-sitter-langs
    :straight t)
 
(use-package all-the-icons
    :straight t)

(use-package treemacs
    :straight t
    :commands (treemacs))

(use-package dired-sidebar
    :straight t
    :general 
    (global-leader-def
        :keymaps 'override
      "t" 'dired-sidebar-toggle-sidebar))

(use-package elec-pair
    :init (electric-pair-mode))

(use-package projectile
    :straight t
    :hook (emacs-startup . projectile-mode)
    :general
    (:keymaps 'prefix-file-map
              "p" 'projectile-find-file)
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

(use-package origami
    :straight t
    :hook (emacs-startup . global-origami-mode))

(use-package zmq
    :straight t
    :defer t)

(use-package markdown-mode
    :straight t)

(use-package iedit
    :straight t
    :general
    (:keymaps 'isearch-mode-map
              "C-;" 'iedit-mode-from-isearch
              ))

(use-package visual-regexp
    :straight t)

(use-package visual-regexp-steroids
    :straight t)

(add-hook 'emacs-startup-hook
          'global-auto-revert-mode)

(require 'elisp-sql-capf)
;; (add-hook 'emacs-startup-hook
;;           'elisp-sql-capf-mode)

(cond
  ((eq system-type 'darwin) (require 'macos)))

(require 'window-conf)
(require 'sexp-conf)
(require 'emacs-lisp-conf)
(require 'selectrum-conf)
(require 'java-conf)
(require 'clojure-conf)
(require 'clisp-conf)
(require 'scheme-conf)
(require 'shell-conf)
(require 'org-conf)
(require 'python-conf)
(require 'docker-conf)
(require 'erlang-conf)
(require 'lsp-conf)
(require 'elixir-conf)
(require 'racket-conf)
(require 'rust-conf)
(require 'c-conf)
(require 'web-conf)
(require 'scala-conf)
(require 'eww-conf)
(require 'swift-conf)
(require 'go-conf)
(require 'ruby-conf)
(require 'sql-conf)
(require 'dotnet-conf)
(require 'latex-conf)
(require 'nim-conf)
(require 'utility-conf)

(load custom-file)
(load (expand-file-name "local.el" user-emacs-directory) nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
