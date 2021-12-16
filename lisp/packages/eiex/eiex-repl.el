;;; -*- lexical-binding: t; -*-

(require 'thingatpt)

(defun eiex-repl-start () 
  (interactive)
  (let ((comint-terminfo-terminal "xterm"))
    (make-comint "eiex-repl" "iex" nil
                 "--sname"
                 "iex@localhost"
                 "--remsh"
                 "eiex@localhost")))

(defun eiex-repl--get-input ()
  (funcall comint-get-old-input))

(defun eiex-repl--get-completions (arg)
  (let ((cpl (eiex-comm-call (vector 'complete arg))))
    (mapcar #'(lambda (h) (gethash 'name h)) cpl)))

(defun eiex-repl-completion-at-point ()
  (let ((bnds (bounds-of-thing-at-point 'symbol)))
    (list
     (car bnds)
     (cdr bnds)
     (completion-table-dynamic 'eiex-repl--get-completions))
    . nil))

(provide 'eiex-repl)
