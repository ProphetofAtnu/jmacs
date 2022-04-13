;;; -*- lexical-binding: t; -*-
(use-package lsp-mssql
    :straight t)

;; (use-package sqlite3
;;     :straight t
;;     :init
;;     (require 'sqlite3))
;; (use-package emacsql
;;     :straight t)

;; (use-package emacsql-libsqlite3
;;     :straight t)


(use-package sql
    :config
  (setq sql-ms-program "sqlcmd")
  (setq sql-ms-options '()))

(add-to-list 'load-path
             (expand-file-name
              "lisp/packages/eodbc"
              user-emacs-directory))
(provide 'sql-conf)
