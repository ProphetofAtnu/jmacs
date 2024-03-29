;; -*- lexical-binding: t; -*-
;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
;; Enable scala-mode for highlighting, indentation and motion commands

(use-package lsp-mode
  :straight t
  ;; Optional - enable lsp-mode automatically in scala files
  :hook  (scala-mode . lsp))

(use-package scala-mode
  :straight t
  :interpreter
  ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :straight t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  ;; (substitute-key-definition
  ;;  'minibuffer-complete-word
  ;;  'self-insert-command
  ;;  minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  )

(use-package lsp-metals
  :straight t
  :defer t)

(provide 'scala-conf)
