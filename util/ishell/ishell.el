;; -*- lexical-binding: t; -*-
(defvar-local ishell-pipe-buffer nil)
(defvar-local ishell-output-buffer nil)
(defvar-local ishell-command-callbacks (make-hash-table))

(defun ishell-make-process-filter-default (proc message)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert message)))

(defmacro ishell--with-saved-position-buffer (&rest body)
  `(save-match-data
    (save-excursion
      (goto-char (point-min))
      ,@body)))


(provide 'ishell)

