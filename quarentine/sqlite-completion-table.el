;; -*- lexical-binding: t; -*-

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))


(setq *sql-obarray* (sqlite-open))


(sqlite-execute *sql-obarray*
 "CREATE TABLE obarray(
symbol TEXT UNIQUE ON CONFLICT IGNORE,
is_func INTEGER
);")

;; (sqlite-execute *sql-obarray* "delete from obarray")

(defvar *last-update* (current-time))
(defvar *debounce-sql-update* 10)

(defun seconds-since-last-update ()
  (time-convert (time-since *last-update*) 'integer))

(defun update-sqlite-obarray ()
  (when (>
         (time-convert (time-since *last-update*) 'integer)
         *debounce-sql-update*)
    (setq *last-update* (current-time))
    (mapatoms
     (lambda (v) (sqlite-execute
             *sql-obarray*
             "INSERT INTO obarray(symbol, is_func) VALUES (?, ?)"
             (list
              (symbol-name v)
              (if (functionp v) 1 0)))))))

(let (atms)
  (mapatoms
   (lambda (v)
     (push (list
            (symbol-name v)
            (if (functionp v) 1 0)
            (if (macrop v) 1 0)
            (if (ignore-errors
                       (or (symbol-value v) t))
                1 0))
           atms)))
  atms)

(sqlite-select *sql-obarray* "select symbol from obarray")

(measure-time
 (mapcar #'car (sqlite-select *sql-obarray* "select symbol from obarray where symbol like 's%'")))

(measure-time
 (let (sym)
   (mapatoms
    (lambda (v) ( when (string-prefix-p "s" (symbol-name v))
             (push v sym))))))


(defun emacs-sql-completion-table (&optional arg &rest ignored)
  (print arg (get-buffer-create "*output*"))
  (if (or (null arg)
          (string-empty-p arg))
      (mapcar
       #'car
       (sqlite-select
        *sql-obarray*
        "select symbol from obarray"))
    (mapcar
     #'car
     (sqlite-select
      *sql-obarray*
      "select symbol from obarray where symbol like ?"
      (list (format "%s%%" arg))))))

(defun emacs-sql-func-completion-table (&optional arg &rest ignored)
  (print arg (get-buffer-create "*output*"))
  (if (or (null arg)
          (string-empty-p arg))
      (mapcar
       #'car
       (sqlite-select
        *sql-obarray*
        "select symbol from obarray where is_func = 1"))
    (mapcar
     #'car
     (sqlite-select
      *sql-obarray*
      "select symbol from obarray where symbol like ? and is_func = 1"
      (list (format "%s%%'" arg))))))

(measure-time
 (all-completions "s" #'emacs-sql-func-completion-table))

(measure-time
 (all-completions "s" obarray #'functionp))

(defvar *update-timer* (run-with-idle-timer .5 nil #'update-sqlite-obarray))

(defun elisp-sql-completion-at-point ()
  (when (or (null *update-timer*)
            (timer--triggered
             *update-timer*))
    (setq *update-timer*
          (run-with-idle-timer
           0.5
           nil
           #'update-sqlite-obarray)))
  (with-syntax-table emacs-lisp-mode-syntax-table
    (let* ((pos (point))
	   (beg (condition-case nil
		    (save-excursion
		      (backward-sexp 1)
		      (skip-chars-forward "`',‘#")
		      (point))
		  (scan-error pos)))
	   (end
	    (unless (or (eq beg (point-max))
			(member (char-syntax (char-after beg))
                                '(?\" ?\()))
	      (condition-case nil
		  (save-excursion
		    (goto-char beg)
		    (forward-sexp 1)
                    (skip-chars-backward "'’")
		    (when (>= (point) pos)
		      (point)))
		(scan-error pos))))
           ;; t if in function position.
           (funpos (eq (char-before beg) ?\())
           (quoted (elisp--form-quoted-p beg))
           (fun-sym (condition-case nil
                        (save-excursion
                          (up-list -1)
                          (forward-char 1)
                          (and (memq (char-syntax (char-after)) '(?w ?_))
                               (read (current-buffer))))
                      (error nil))))
      (when (and end (or (not (nth 8 (syntax-ppss)))
                         (memq (char-before beg) '(?` ?‘))))
        (if (or (not funpos) quoted)
            (list beg end
                  (apply-partially #'completion-table-with-terminator " "
                                   (lambda (&rest _)
                                     (emacs-sql-completion-table
                                      (buffer-substring-no-properties
                                       beg
                                       end)))))
          (list
           beg
           end
           (apply-partially
            #'completion-table-with-terminator
            " "
            (lambda (&rest _)
              (emacs-sql-completion-table
               (buffer-substring-no-properties
                beg
                end))))))
        ))))



(all-completions "setq" #'emacs-sql-completion-table)
(all-completions "" obarray)

(sqlite-select *sql-obarray* "select * from obarray where symbol like 'setq%'")

(length obarray)
(intern "This is a test")
