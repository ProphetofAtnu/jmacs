;;; -*- lexical-binding: t; -*-
(module-load
 (expand-file-name
  "target/debug/libeodbc.so"
  (file-name-directory load-file-name)))

(require 'eodbc-mssql)

(defvar-local eodbc--buffer-cache (make-hash-table))
(defvar-local eodbc-buffer-dsn nil)
(defvar-local eodbc-buffer-db nil)

(defconst eodbc-db-cache-kinds
  '(:table :view :column :routine))

(provide 'emacs-odbc)
