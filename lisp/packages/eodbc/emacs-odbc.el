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

(defun eodbc--output-table-buffer (data)
  (with-current-buffer (generate-new-buffer "*eodbc-result*")
    (dolist (row data) 
      (insert (format "|%s|\n" (string-join row "|"))))
    (org-table-align)
    (goto-char (point-min))
    (org-table-insert-hline)
    (toggle-truncate-lines 1)
    (display-buffer (current-buffer))))

(provide 'emacs-odbc)
