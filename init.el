;;; -*- lexical-binding: t; -*-

(defvar use-package-compute-statistics t)

(defvar native-comp-deferred-compilation-deny-list nil)


(require 'early/bootstrap-straight)
;; Set up use-package
(straight-use-package 'use-package)
(require 'use-package)

(require 'early/vars)
(require 'early/packages)
(require 'early/load-paths)
(require 'early/core)
(require 'early/interactive)

(byte-recompile-directory (expand-file-name "util" user-emacs-directory) 0)
(require 'core/utility)
;;;; Platform specific setup
(cond
 ((eq system-type 'darwin) (require 'macos))
 (t (progn (set-face-attribute 'default t :font "FiraCode Nerd Font-12")
           (set-face-font 'fixed-pitch "FiraCode Nerd Font-12")
           (set-frame-font "FiraCode Nerd Font-12" nil t))))

;; Global Configurations
(require 'global/global-tools)
(require 'global/global-edit-tools)
(require 'global/global-evil)
(require 'global/global-completion-styles)
(require 'global/global-which-key)
(require 'global/global-tree-sitter)

(require 'window-conf) ;; TODO: Move to global configurations

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

;; Load the global themes
(require 'global/global-theme)

(load (custom-file))
(load (expand-file-name "local.el" user-emacs-directory) nil)

(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
