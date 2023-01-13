;; -*- lexical-binding: t; -*-


;;;; Note!
;; Only things that are depended on by INTERACTIVE code should end up
;; in this file. 

;; UI packages that should be globally available
(straight-use-package 'general)
(straight-use-package 'hydra)
(straight-use-package 'svg-lib)
(straight-use-package 'avy)
(straight-use-package 'dired-hacks)
(straight-use-package 'tablist)
(straight-use-package 'switch-window)
(straight-use-package 'emr)
(straight-use-package 'elixir-mode)
(straight-use-package 'markdown-mode)

;; For general
(straight-use-package 'evil)
(straight-use-package 'evil-mc)

;; Configuration
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(setq warning-minimum-level :error
      warning-minimum-log-level :error
      native-comp-async-query-on-exit t)

(setq create-lockfiles nil)

(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


(setq warning-minimum-level :error
      warning-minimum-log-level :error
      native-comp-async-query-on-exit t)

(setq create-lockfiles nil)

(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq visible-bell nil
      frame-resize-pixelwise t
      window-resize-pixelwise t
      ring-bell-function #'ignore
      inhibit-startup-screen t
      comint-prompt-read-only t
      sentence-end-double-space nil
      minibuffer-follows-selected-frame nil
      tab-width 4
      byte-compile-warnings nil
      custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'autorevert)
(add-hook 'emacs-startup-hook
          'global-auto-revert-mode)

(with-eval-after-load 'prog-mode
  (add-hook 'prog-mode-hook 'prettify-symbols-mode))

(use-package exec-path-from-shell
  :straight t
  :hook (emacs-startup . exec-path-from-shell-initialize)
  :init
  (setq exec-path-from-shell-variables
        '("LDFLAGS" "CPPFLAGS" "PATH" "MANPATH")))

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

(use-package tab-bar
  :config
  (setq tab-bar-show nil))

(use-package no-littering
  :straight t
  :demand t)

(use-package delight
  :straight t
  :demand t)

(use-package popup
  :straight t
  :demand t)

(use-package exec-path-from-shell
  :straight t
  :hook (emacs-startup . exec-path-from-shell-initialize)
  :init
  (setq exec-path-from-shell-variables
        '("LDFLAGS" "CPPFLAGS" "PATH" "MANPATH")))

(use-package recentf
  :hook (emacs-startup . recentf-mode))


(provide 'early/interactive)
