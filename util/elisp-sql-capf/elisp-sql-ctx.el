;; -*- lexical-binding: t; -*-


(defun elisp-sql-ctx--paren-capture (point)
  (let ((prev-quote (eq ?' (char-syntax (char-before)))))
    (goto-char point)
    (forward-char 1)
    (list
     prev-quote
     (intern-soft
      (buffer-substring-no-properties
       (point)
       (progn
         (forward-sexp)
         (point)))))))

(defun elisp-sql-ctx-capture ()
  (let (captures
        (parens (nth 9 (syntax-ppss))))
    (save-excursion
      (dolist (ploc parens captures)
        (push (cons ploc (elisp-sql-ctx--paren-capture ploc))
              captures)))))

(provide 'elisp-sql-ctx)
