;;; elisp-sql-capf.el --- Elisp completion-at-point sql extension  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Jacob Scaggs

;; Author: Jacob Scaggs
;; Keywords: lisp, tools, extensions
;; Package-Requires: ((emacs "29.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;  This package is essentially a toy completion function to see if
;;  performance could be improved using some of the newer features in
;;  Emacs 29. 

;; Activating the global minor mode `elisp-sql-capf-mode' replaces the
;; default elisp-completion-at-point function with
;; `elisp-sql-completion-at-point'.

;;; Code:

(require 'elisp-mode)

(cl-assert (sqlite-available-p) nil "elisp-sql: Emacs needs to be compiled with sqlite suport.")
(cl-assert (functionp 'make-thread) nil "elisp-sql: Emacs needs to be compiled with thread suport.")

(defconst elisp-sql-sp-flag (ash 1 4))
(defconst elisp-sql-kw-flag (ash 1 3))
(defconst elisp-sql-macro-flag (ash 1 2))
(defconst elisp-sql-function-flag (ash 1 1))
(defconst elisp-sql-bound-flag (ash 1 0))

(defvar elisp-sql-database-file-type 'file
  "Either `memory' or `file', depending on the storage strategy the
database should use.")

(defvar elisp-sql-persistant-file
  (expand-file-name "var/elisp-sqlite.db" user-emacs-directory))

(defvar elisp-sql--database-memory-name ":memory:")

(defvar elisp-sql-debounce-time 5)
(defvar elisp-sql-verbose nil)

(defvar *elisp-sql-update-thread* nil)
(defvar *elisp-sql-update-timer* nil)

(defvar *elisp-sql-obarray* nil)
(defvar *elisp-sql-registered* (make-hash-table))
(defvar *elisp-sql-update-ts* (float-time))

(defun elisp-sql-log (str &rest args)
  (when elisp-sql-verbose
    (with-current-buffer (get-buffer-create "*elisp-sql-log*")
      (goto-char (point-max))
      (insert (concat (current-time-string) " -> " (apply #'format str args)))
      (newline))))


(defvar elisp-sql-debug-assert-sqlite-macro t
  "If enabled, `with-sqlite-connection' asserts the sqlite DB closes
with `assert-db-closed'.")

(defun assert-db-closed (db)
  (condition-case e
      (sqlite-execute db "SELECT 1")
    (error
     (string=
      (cadr e)
      "Database closed"))))

(defmacro with-sqlite-connection (db uri &rest body)
  (let ((sym (gensym "sqlc")))
    `(let ((,db (sqlite-open ,uri))
           (,sym nil))
       (unwind-protect
            (setf ,sym (progn ,@body))
         (sqlite-close ,db))
       ,sym)))

(defun elisp-sql-db-uri ()
  (cl-case elisp-sql-database-file-type 
    ('file elisp-sql-persistant-file)
    ('memory elisp-sql--database-memory-name)))

(defun elisp-sql--ensure-file-dir ()
  (unless (file-exists-p
           (file-name-directory
            elisp-sql-persistant-file))
    (make-directory
     (file-name-directory
      elisp-sql-persistant-file)
     t)))
           
(defun elisp-sql--check-db (db)
  (member "symbols" (mapcar
                   #'car
                   (sqlite-select db
                    "SELECT name FROM pragma_table_list"))))

(defun elisp-sql-open-db ()
  (when (eq  elisp-sql-database-file-type 'file)
    (elisp-sql--ensure-file-dir))
  (sqlite-open (elisp-sql-db-uri)))

(defun create-sql-obarray ()
  (let ((db (elisp-sql-open-db)))
    (sqlite-pragma db "case_sensitive_like=1")
    (unless (elisp-sql--check-db db)
      (sqlite-execute
       db
       "CREATE TABLE symbols(name TEXT,flags INTEGER);")
      (sqlite-execute
       db
       "CREATE INDEX symbols_name_types ON symbols(name, flags)")
      (elisp-sql-log "Initialized database, table + index"))
    (cl-assert (elisp-sql--check-db db))
    db))


(defun elisp-sql-calculate-flags (sym)
  (logior 
   (ash (if (boundp sym) 1 0) 0)
   (ash (if (functionp sym) 1 0) 1)
   (ash (if (macrop sym) 1 0) 2)
   (ash (if (keywordp sym) 1 0) 3)
   (ash (if (special-form-p sym) 1 0) 4)))

(defun update-sql-obarray (db)
  (condition-case e
      (progn
        (elisp-sql-log "Starting update transaction...")
        (sqlite-transaction db)
        ;; (sqlite-execute db "DELETE FROM symbols")
        (mapatoms
         (lambda (i)
           (when (and
                  (symbolp i)
                  (gethash i *elisp-sql-registered* t))
             (puthash i nil *elisp-sql-registered*)
             (let ((n (symbol-name i))
                   (flags (elisp-sql-calculate-flags i)))
               (when (and n (> flags 0))
                 (sqlite-execute
                  db
                  "INSERT INTO symbols(name, flags) VALUES (?, ?)"
                  (list n flags)))))))
        (sqlite-commit db)
        (elisp-sql-log "Committed symbol update")
        'ok)
    (error (progn
             (elisp-sql-log "Rolled back update %s" e)
             (sqlite-rollback db)
             nil))))

(defun elisp-sql--reset-file-db-hard ()
  (sqlite-close *elisp-sql-obarray*)
  (delete-file elisp-sql-persistant-file)
  (elisp-sql-log "Reset obarray file: %s, exists?; %s"
                 elisp-sql-persistant-file
                 (file-exists-p elisp-sql-persistant-file))
  (setq *elisp-sql-obarray* (create-sql-obarray)))

(defun elisp-sql-last-updated-time ()
  (- (float-time) *elisp-sql-update-ts*))

(cl-defun elisp-sql--apply-properties ((li fl))
  (propertize li 'symbol-flags
              (concat
               (if (> (logand fl elisp-sql-bound-flag) 0) "b")
               (if (> (logand fl elisp-sql-function-flag) 0) "f")
               (if (> (logand fl elisp-sql-macro-flag) 0) "m")
               (if (> (logand fl elisp-sql-kw-flag) 0) "k")
               (if (> (logand fl elisp-sql-sp-flag) 0) "s")
               )))

(defun elisp-sql-complete-with-flags (db prefix &rest flags)
  (let ((flags-value (logior flags)))
    (cl-loop for x in (sqlite-select db
                                     "SELECT name, flags FROM symbols WHERE (flags & ?) = ? and name like ?"
                                     (list flags-value flags-value (concat prefix "%")))
          collect (elisp-sql--apply-properties x))
    ))


(defun elisp-sql-complete-symbol (db prefix)
  (mapcar #'elisp-sql--apply-properties
          (sqlite-select
           db
           "SELECT name, flags FROM symbols WHERE flags > 0 and name like ?"
           (list (concat prefix "%")))))

;; (defun elisp-sql-complete-symbol (db prefix)
;;   (cl-loop for x in 
;;         (sqlite-select
;;          db
;;          "SELECT name, flags FROM symbols WHERE flags > 0 and name like ?"
;;          (list (concat prefix "%")))
;;         collect (elisp-sql--apply-properties x)))

(defun elisp-sql-complete-func (db prefix)
  (mapcar #'car
          (sqlite-select
           db
           "SELECT name, flags FROM symbols WHERE (flags & 2) AND name LIKE ?"
           (list (concat prefix "%")))))

(defun elisp-sql-complete-callable (db prefix)
  (mapcar #'car
          (sqlite-select
           db
           "SELECT name, flags FROM symbols WHERE (flags & 6) > 1 and name LIKE ?"
           (list (concat prefix "%")))))

(defun elisp-sql-complete-kw (db prefix)
  (mapcar #'car
          (sqlite-select
           db
           "SELECT name, flags FROM symbols WHERE (flags & 8) > 1 and name LIKE ?"
           (list (concat prefix "%")))))

(defun elisp-sql-complete-bound (db prefix)
  (mapcar #'car
          (sqlite-select
           db
           "SELECT name, flags FROM symbols WHERE (flags & 1) > 0 and name LIKE ?"
           (list (concat prefix "%")))))

(defun elisp-sql-benchmark ()
  (interactive)
  (with-output-to-temp-buffer
      "*tmp*"
    (princ
     (format
      "Update from obarray: %s\n"
      (benchmark
               1
             '(update-sql-obarray
               *elisp-sql-obarray*))))
    (princ
     (format
      "Query speed of sqlite database for function completion: %s\n"
      (benchmark
               1000
             '(elisp-sql-complete-func
               *elisp-sql-obarray*
               "s"))))
    (princ
     (format
      "Query speed of basic AC functions for function completion: %s\n"
      (benchmark
               1000
             '(all-completions
               "s"
               obarray
               #'functionp))))))

(defun elisp-sql-sexp-quote-kind (point)
  (let ( (prev (char-before point))
        (pprev (char-before (1- point))))
    (if (or (eq prev ?\')
            (eq prev ?\`))
        (if (eq pprev ?\#)
            :function
          :quoted)
      (if (eq prev ?\()
          :car))))

(defun elisp-sql--paren-info ()
  (let ((state (syntax-ppss)))
    (cons (ppss-last-complete-sexp-start state)
          (reverse (nth 9 (syntax-ppss))))))

(defun elisp-sql--state-nesting-info (state)
  (cons
   (if (zerop (ppss-depth state))
       (elisp-sql--start-boundry)
     (or (ppss-last-complete-sexp-start state)
         (1+ (ppss-innermost-start state))))
   (reverse (nth 9 (syntax-ppss)))))

(defun elisp-sql--check-context ()
  (let ((state (syntax-ppss)))
    (unless (syntax-ppss-context state)
      (mapcar
       #'(lambda (pos) (cons pos (elisp-sql-sexp-quote-kind pos)))
       (elisp-sql--state-nesting-info
        state)))))

(defun elisp-sql--start-boundry ()
  (save-excursion
    (skip-syntax-backward "w_")
    (point)))

(defun elisp-sql--end-boundry ()
  (save-excursion
    (skip-syntax-forward "w_")
    (point)))

(defun elisp-sql-context-immediate ()
  (car (elisp-sql--check-context)))


(defun elisp-sql-completer (prefix calling)
  (lambda (&rest ignored)
    (funcall calling *elisp-sql-obarray* prefix)))

(defun elisp-sql--annotate (cap)
  (when-let ((flg (get-text-property 0 'symbol-flags cap)))
    (format "   (%s)" flg)))

(defun elisp-sql-check-update-deferred (&optional from-timer)
  (when (and
         (or from-timer (null *elisp-sql-update-timer*))
         (not (and *elisp-sql-update-thread*
                   (thread-live-p *elisp-sql-update-thread*)))
         (> (- (float-time) *elisp-sql-update-ts*)
            elisp-sql-debounce-time))
    (setq *elisp-sql-update-thread*
          (make-thread 
           #'(lambda ()
               (with-sqlite-connection
                   db (elisp-sql-db-uri)
                   (cl-case
                       (while-no-input (update-sql-obarray db))
                     ('ok (progn
                            (elisp-sql-log "Update thread completed")
                            (when *elisp-sql-update-timer*
                              (cancel-timer *elisp-sql-update-timer*)
                              (setq *elisp-sql-update-timer* nil))
                            (setq *elisp-sql-update-ts* (float-time))))
                     (t (progn
                          (sqlite-rollback db)
                          (elisp-sql-log "Update thread rescheduled")
                          (unless *elisp-sql-update-timer*
                            (setq *elisp-sql-update-timer*
                                  (run-with-idle-timer
                                   1 t #'elisp-sql-check-update-deferred t))))))))
           "Elisp Obarray Updates"))))

(defun elisp-sql-completion-at-point ()
  (elisp-sql-check-update-deferred)
  (if-let ((ctx (elisp-sql-context-immediate))
           (start (elisp-sql--start-boundry))
           (end (elisp-sql--end-boundry)))
      (cl-destructuring-bind (beg . kind) ctx
        (list start end
              (completion-table-dynamic
               (elisp-sql-completer
                (buffer-substring-no-properties
                 start end)
                (cl-case kind
                  (:function #'elisp-sql-complete-func)
                  (t #'elisp-sql-complete-symbol))))
              :annotation-function #'elisp-sql--annotate
              :company-kind #'elisp--company-kind
              :company-doc-buffer #'elisp--company-doc-buffer
              :company-docsig #'elisp--company-doc-string
              :company-location #'elisp--company-location
              :company-deprecated #'elisp--company-deprecated))))

(define-minor-mode elisp-sql-capf-mode
    "Minor mode to override the default elisp CAPF function with a
sqlite-based one. Requires Emacs to be compiled with sqlite3
support, and a minimum libsqlite3 version of 3.31.0 (for
GENERATED columns)."
  :global t
  (if elisp-sql-capf-mode
      (progn
        (unless *elisp-sql-obarray*
          (setq *elisp-sql-obarray* (create-sql-obarray)))
        (elisp-sql-check-update-deferred)
        (advice-add 'elisp-completion-at-point :override #'elisp-sql-completion-at-point))
    (advice-remove 'elisp-completion-at-point #'elisp-sql-completion-at-point)))


(provide 'elisp-sql-capf)
;;; elisp-sql-capf.el ends here
