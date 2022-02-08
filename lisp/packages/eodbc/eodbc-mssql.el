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

(provide 'eodbc-mssql)
