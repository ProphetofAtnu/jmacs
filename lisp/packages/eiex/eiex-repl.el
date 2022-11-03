;;; -*- lexical-binding: t; -*-

(require 'thingatpt)
(require 'eiex-comm)

(defun eiex-repl-start () 
  (interactive)
  (let ((comint-terminfo-terminal "xterm"))
    (make-comint "eiex-repl" "iex" nil
                 "--sname"
                 "iex@localhost"
                 "--remsh"
                 "eiex@localhost")))

(defun eiex-repl--complete-bounds ()
  (let ((ctx (syntax-bounds "w_."))
        (crng (syntax-bounds "w_")))
    (list ctx crng)))

(defun eiex-repl--propertize-candidate (cand)
  (pcase cand
    (`(,s fun ,ar ,mod) (propertize s
                                    'type 'fun
                                    'arity ar
                                    'module mod))
    (`(,s mod) (propertize s
                           'type 'mod))))

(defun eiex-repl--do-complete (&optional str)
  (cl-destructuring-bind (ctx cpl) (eiex-repl--complete-bounds)
    (let ((candidates (eiex-comm-complete (apply 'buffer-substring-no-properties ctx))))
      (mapcar #'eiex-repl--propertize-candidate candidates))))

(defun eiex-repl--do-annotate (item)
  (if-let ((ar (get-text-property 0 'arity item)))
      (string-join 
       (mapcar #'(lambda (a) (format " &/%s" a))
            (string-to-list ar))
       "")))

(defun eiex-repl-completion-at-point ()
  (cl-destructuring-bind (ctx cpl) (eiex-repl--complete-bounds)
    (list 
     (car cpl)
     (cadr cpl)
     (completion-table-dynamic #'eiex-repl--do-complete)
     :annotation-function #'eiex-repl--do-annotate)))

(provide 'eiex-repl)
