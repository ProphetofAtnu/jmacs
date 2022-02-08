;;; -*- lexical-binding: t; -*-

(use-package sql
    :config
  (setq sql-ms-program "sqlcmd"))

(add-to-list 'load-path
             (expand-file-name
              "lisp/packages/eodbc"
              user-emacs-directory))

(provide 'sql-conf)
