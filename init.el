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
(straight-use-package 'dash)
(straight-use-package 'evil)
(straight-use-package 'switch-window)
(straight-use-package 'emr)
(straight-use-package 'evil-mc)
(straight-use-package 'elixir-mode)
(straight-use-package 'emacsql)
(straight-use-package 'emacsql-sqlite-builtin)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)


(setq warning-minimum-level :error
      warning-minimum-log-level :error
      native-comp-async-query-on-exit t)

(setq create-lockfiles nil)

(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "util" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "util/tools" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "util/elisp-sql-capf" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "platform" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/packages/epy" user-emacs-directory))


(setq visible-bell nil
      frame-resize-pixelwise t
      window-resize-pixelwise t
      ring-bell-function #'ignore
      inhibit-startup-screen t
      comint-prompt-read-only t
      sentence-end-double-space nil
      gc-cons-threshold 100000000
      minibuffer-follows-selected-frame nil
      tab-width 4
      read-process-output-max (* 1024 1024)
      byte-compile-warnings nil
      custom-file (expand-file-name "custom.el" user-emacs-directory))

(byte-recompile-directory (expand-file-name "util" user-emacs-directory) 0)
;; (byte-recompile-directory user-emacs-directory)
;; (byte-recompile-directory (expand-file-name "lisp/core" user-emacs-directory) 0)

(defvar js/should-load-and-compile-directories
  (list
   (expand-file-name "util" user-emacs-directory)
   ;; (expand-file-name "lisp/core" user-emacs-directory)
   ))


(use-package exec-path-from-shell
  :straight t
  :hook (emacs-startup . exec-path-from-shell-initialize)
  :init
  (setq exec-path-from-shell-variables
        '("LDFLAGS" "CPPFLAGS" "PATH" "MANPATH")))


(use-package no-littering
  :straight t)

(use-package dash
  :straight t)

(use-package delight
  :straight t)

(use-package popup
  :straight t)

(require 'core/utility)

;; Evil

(defvar evil-want-integration t)
(defvar evil-disable-insert-state-bindings t)
(defvar evil-want-keybinding nil)

(use-package evil 
  :straight t
  :delight
  :hook (emacs-startup . evil-mode)
  :init 
  (setq evil-want-C-u-scroll t
        evil-cross-lines t
        evil-undo-system 'undo-redo
        evil-ex-search-vim-style-regexp t
        evil-search-module 'evil-search))

(use-package evil-collection
  :straight t
  :init
  (evil-collection-init)
  (global-evil-collection-unimpaired-mode +1)
  :config
  (delq 'lispy evil-collection-mode-list))

(use-package evil-goggles
  :straight t
  :delight
  :hook (evil-mode . evil-goggles-mode)
  :config
  (evil-goggles-use-diff-faces))

(use-package evil-surround
  :straight t
  :after (evil)
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
  :hook (evil-mode . global-evil-matchit-mode))

(use-package evil-lion
  :straight t
  :hook (evil-mode . evil-lion-mode))

(use-package hydra
  :straight t)

(use-package evil-commentary
  :straight t
  :after (evil)
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
  :hook (emacs-startup . which-key-mode)
  :config
  (add-to-list 'which-key-replacement-alist
               '((nil . "evil-collection-\\(.*\\)") . (nil . "ec-\\1"))))

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
  (require 'core/bindings)
  :config
  (with-eval-after-load 'evil
    (general-defs
      :states '(normal motion)
      "M-u" 'universal-argument)
    ))


(use-package orderless
  :straight t
  :ensure t
  :commands (orderless-filter))

(use-package fussy
  :straight t
  :config
  (push 'fussy completion-styles)
  (setq fussy-filter-fn 'fussy-filter-orderless)
  (setq
   ;; For example, project-find-file uses 'project-files which uses
   ;; substring completion by default. Set to nil to make sure it's using
   ;; flx.
   completion-category-defaults nil
   completion-category-overrides nil))

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
        corfu-auto-delay 0.2
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
  (global-corfu-mode)
  )

(use-package cape
  :straight t
  :config ;; Silence the pcomplete capf, no errors or messages!
  (advice-add
   'pcomplete-completions-at-point
   :around #'cape-wrap-silent)
  (dolist (i 
           '(evil-ex-elisp-completion-at-point
             evil-ex-command-completion-at-point
             evil-ex-argument-completion-at-point))
    (advice-add
     i
     :around #'cape-wrap-silent))
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
  :defer nil
  :hook (evil-mode . global-evil-mc-mode)
  :general
  (:keymaps '(evil-mc-key-map)
            :states '(normal)
            "z ." 'evil-mc-make-cursor-here 
            "z <" 'evil-mc-pause-cursors
            "z >" 'evil-mc-resume-cursors
            "z /" 'evil-mc-undo-all-cursors
            )
  :init
  (require 'core/hydras)
  (general-def
    :states '(normal)
    "g.." 'evil-mc-hydra/body)
  (evil-mc-define-vars)
  (add-to-list 'evil-mc-incompatible-minor-modes 'lispy)
  ) 


(use-package evil-avy
  :straight t
  :after (evil)
  ;; :hook (evil-mode . evil-avy-mode)
  :general
  (:states '(normal)
           "s" 'evil-avy-goto-char-2))

(use-package evil-snipe
  :straight t
  :config
  (setq evil-snipe-scope 'whole-buffer
        evil-snipe-enable-highlight t
        evil-snipe-enable-incremental-highlight t
        evil-snipe-show-prompt nil
        evil-snipe-smart-case t)
  (evil-snipe-mode 1))

(use-package company
  :straight t
  :general
  (:keymaps 'company-active-map
            "TAB" 'company-complete-common-or-cycle
            "<tab>" 'company-complete-common-or-cycle)
  ;; (:states '(insert)
  ;;          "TAB" 'company-indent-or-complete-common
  ;;          "<tab>" 'company-indent-or-complete-common
  ;;          )
  ;; :init
  ;; (add-hook 'emacs-startup-hook
  ;;           'global-company-mode)
  :config
  (add-hook 'company-mode-hook (lambda () (corfu-mode -1)))
  (defun company-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (company-mode 1)))
  (delq 'company-semantic company-backends)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1))


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

;; straight-use-package
;; (use-package parrot
;;     :straight
;;   (parrot :type git :host github :repo "dp12/parrot")
;;   )

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

(use-package telephone-line
  :straight t
  :config
  (delight 'emacs-lisp-mode "Elisp")
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (setq telephone-line-height 24
        telephone-line-evil-use-short-tag t)
  (telephone-line-mode +1))

(use-package kaolin-themes
  :straight t
  :defer t)

(use-package cherry-blossom-theme
  :straight t
  :defer t)

(use-package modus-themes
  :straight t
  :config
  (load-theme 'modus-vivendi t))

(use-package zoxide
  :straight t
  :general
  (prefix-file-map
   "z" 'zoxide-find-file
   "t" 'zoxide-travel
   "c" 'zoxide-cd))

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

;; (use-package dired-filter)
;; (use-package dired-narrow)
;; (use-package dired-subtree)

(use-package tempel
  :straight t
  :bind (("M-+" . tempel-complete)
         ;; Alternative tempel-expand
         ("M-*" . tempel-insert)))


(use-package yasnippet
  :straight t
  :hook (emacs-startup . yas-global-mode)
  :config
  ;; (general-def
  ;;     'yas-minor-mode-map
  ;;     "SPC"
  ;;   yas-maybe-expand)
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

(use-package string-inflection
  :straight t
  :config
  (general-defs
    :keymaps 'prefix-edit-map
    "i" (js/to-repeatable string-inflection-all-cycle)))

(use-package tree-sitter
  :straight t
  :hook (emacs-startup . global-tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-indent
  :straight t)

(use-package tree-sitter-langs
  :straight t)

(use-package all-the-icons
  :straight t)

(use-package treemacs
  :straight t
  :commands (treemacs))

(use-package winner
  :general
  ('winner-mode-map
   "C-M-<" 'winner-undo
   "C-M->" 'winner-redo
   )
  :hook (emacs-startup . winner-mode))

(use-package smartparens
  :straight t
  :hook (emacs-startup
         . smartparens-global-mode)
  :config 
  (require 'smartparens-config)
  (setq sp-highlight-wrap-overlay t
        sp-highlight-pair-overlay nil
        sp-highlight-wrap-tag-overlay t))

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
            "r" 'projectile-ripgrep
            "b" 'consult-projectile-switch-to-buffer
            "!" 'projectile-cleanup-known-projects
            "&" 'projectile-run-async-shell-command-in-root
            "w" 'projectile-run-term
            "o" 'projectile-find-other-file
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

(add-hook 'emacs-startup-hook
          'global-auto-revert-mode)

(cond
 ((eq system-type 'darwin) (require 'macos))
 (t (progn (set-face-attribute 'default t :font "FiraCode Nerd Font-12")
           (set-face-font 'fixed-pitch "FiraCode Nerd Font-12")
           (set-frame-font "FiraCode Nerd Font-12" nil t))))

(require 'window-conf)
(require 'sexp-conf)
(require 'emacs-lisp-conf)
(require 'selectrum-conf)
(require 'lsp-conf)
(require 'java-conf)
(require 'clojure-conf)
(require 'clisp-conf)
(require 'scheme-conf)
(require 'shell-conf)
(require 'org-conf)
(require 'python-conf)
(require 'docker-conf)
(require 'erlang-conf)
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
(require 'ocaml-conf)
(require 'zig-conf)
(require 'latex-conf)
(require 'nim-conf)
(require 'haskell-conf)
(require 'flutter-conf)
(require 'julia-conf)
(require 'utility-conf)

(load custom-file)
(load (expand-file-name "local.el" user-emacs-directory) nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
