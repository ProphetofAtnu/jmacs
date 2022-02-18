;;; -*- lexical-binding: t; -*-

(use-package sql
    :config
  (setq sql-ms-program "sqlcmd")
  (setq sql-ms-options '()))

(add-to-list 'load-path
             (expand-file-name
              "lisp/packages/eodbc"
              user-emacs-directory))

(provide 'sql-conf)
