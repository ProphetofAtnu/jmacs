;; -*- lexical-binding: t; -*-


(defvar symbol-match-table-schema
  "CREATE VIRTUAL TABLE symbols_lookup USING fts5(name, sym_id, flags, keywords, var_docs, func_docs);")

(defvar symbol-obarray-table-schema
  "CREATE TABLE symbols(
sym_id INTEGER PRIMARY KEY AUTOINCREMENT,
name UNIQUE ON CONFLICT IGNORE,
flags INTEGER,
in_fts INTEGER DEFAULT 0)")

(defvar symbol-match-update-query
  "INSERT INTO symbols_lookup(name, sym_id, flags)
SELECT name, sym_id, flags FROM symbols
WHERE flags > 0 
AND in_fts < 1")

(defvar symbol-match-post-update-query
  "UPDATE symbols SET in_fts = 1")

(defun elisp-sql-update-match-symbols (db)
  (unwind-protect
       (progn
         (sqlite-transaction db)
         (sqlite-execute db symbol-match-update-query)
         (sqlite-execute db symbol-match-post-update-query)
         (sqlite-commit db))
    (sqlite-rollback db))
  )

(defun bool-number (value)
  (if value 1 0))

(defun symbol-flags (sym)
  (logior
   (ash (bool-number
         (boundp sym))
        0)
   (ash (bool-number
         (special-form-p sym))
        1)
   (ash (bool-number
         (functionp sym))
        2)
   (ash (bool-number
         (macrop sym))
        3)
   (ash (bool-number
         (keywordp sym))
        4)))

(defun read-symbol-func-docs (sym)
  (substring-no-properties
   (describe-function sym)))

(defun read-symbol-var-docs (sym)
  (substring-no-properties
   (describe-variable sym)))

(defun elisp-sql-insert-symbol (db sym)
  (sqlite-execute
   db
   "INSERT INTO symbols(name, flags) VALUES (?, ?);"
   (list (symbol-name sym)
         (symbol-flags sym)))
  )

(defun elisp-sql-insert-symbol-data (db data)
  (sqlite-execute
   db
   "INSERT INTO symbols(name, flags) VALUES (?, ?);" data))

(defun elisp-sql-collect-symbols ()
  (cl-loop for sym being the symbols
        collect (list (symbol-name sym)
                      (symbol-flags sym))))

(defun elisp-sql-update-in-bg (db &optional on-done)
  (let ((now (current-time))
        (timer (timer-create))
        (syms (elisp-sql-collect-symbols)))
    (timer-set-time timer (timer-relative-time nil 0) 0.01)
    (timer-set-function timer
                        (lambda ()
                          (if (eq :done (while-no-input
                                          (while (consp syms)
                                            (elisp-sql-insert-symbol-data
                                             db
                                             (car syms))
                                            (pop syms))
                                          :done))
                              (progn
                                (run-with-idle-timer
                                 0.1 nil #'elisp-sql-update-match-symbols db)
                                (cancel-timer timer)
                                (when (functionp on-done)
                                  (funcall on-done))))
                          ))
    (timer-activate timer)
    timer))

(cl-defstruct elisp-symbol-database
  handle last-check-count update-timer)


(provide 'elisp-sql)
