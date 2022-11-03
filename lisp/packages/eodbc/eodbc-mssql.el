;;; -*- lexical-binding: t; -*-

(require 'eodbc)

(defun eodbc-mssql-get-dbs (dsn)
  (flatten-list (cdr
                 (eodbc-run-sql-to-lisp
                  dsn
                  "SELECT name FROM sys.databases"))))

(defun eodbc-mssql-get-db-schemas (dsn db)
 (flatten-list (cdr
                (eodbc-run-sql-to-lisp-in-db
                 dsn
                 db
                 "SELECT name FROM sys.schemas"))))

(defun eodbc-mssql-get-db-tables (dsn db)
 (cdr
                (eodbc-run-sql-to-lisp-in-db
                 dsn
                 db
                 "SELECT TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME FROM INFORMATION_SCHEMA.TABLES")))

(defun eodbc-mssql-get-db-views (dsn db)
 (cdr
                (eodbc-run-sql-to-lisp-in-db
                 dsn
                 db
                 "SELECT TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME FROM INFORMATION_SCHEMA.VIEWS")))

(defun eodbc-mssql-get-db-columns (dsn db)
  (cdr
   (eodbc-run-sql-to-lisp-in-db
    dsn db
    "SELECT TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS")))


(defun eodbc-mssql-get-db-routines (dsn db)
  (cdr (eodbc-run-sql-to-lisp-in-db
        dsn
        db
        "SELECT ROUTINE_CATALOG,ROUTINE_SCHEMA,ROUTINE_NAME,ROUTINE_TYPE FROM INFORMATION_SCHEMA.ROUTINES")))


(defconst eodbc-mssql--statement-rx
  (rx (seq
       (or
        "SELECT"
        "DELETE"
        "UPDATE"
        "MERGE"
        "ALTER"
        "INSERT")
       (not word))))

(defconst eodbc-mssql--statement-end-rx
  (rx (or
       "GO"
       ";")))

(defun eodbc-mssql--get-statement-start ()
  (save-excursion
    (save-match-data
      (let* ((case-fold-search t)
             (start
              (ignore-errors (re-search-backward eodbc-mssql--statement-rx))))
        start))))

(defun eodbc-mssql--get-statement-end (&optional from-point)
  (save-excursion
    (save-match-data
      (when from-point
        (goto-char from-point))
      (let* ((case-fold-search t)
             (start
              (ignore-errors (re-search-forward eodbc-mssql--statement-end-rx))))
        start))))


(defun eodbc-mssql--statement-at-or-before-point ()
  (if-let* ((start (eodbc-mssql--get-statement-start))
            (end (or (eodbc-mssql--get-statement-end start)
                     (point-max))))
      (buffer-substring-no-properties start end)))

(defun eodbc-mssql-eval-statement ()
  (interactive)
  (if-let ((prev (eodbc-mssql--statement-at-or-before-point))
           (dsn eodbc-buffer-dsn))
      (if eodbc-buffer-db
          (eodbc--output-table-buffer (eodbc-run-sql-to-lisp-in-db dsn eodbc-buffer-db prev))
          (eodbc--output-table-buffer (eodbc-run-sql-to-lisp dsn prev)))))

(provide 'eodbc-mssql)
