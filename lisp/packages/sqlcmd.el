;;; -*- lexical-binding: t; -*-

(defvar sqlcmd-program "sqlcmd")
(defvar sqlcmd-options '("-W" "-s" "|"))

(cl-defun
    sqlcmd-make-options
    (&key
       server (db nil) (user nil) (pass nil)
       (dsn nil) (integrated t))
  (let ((args (cl-copy-list sqlcmd-options)))
    (when db
      (setf
       (cdr (last args))
       (list "-d" db)))
    (when user
      (setf
       (cdr (last args))
       (list "-U" user)))
    (when pass
      (setf
       (cdr (last args))
       (list "-P" pass)))
    (when dsn (push "-D" args))
    (when integrated
      (push "-E" args))
    (append
     (list "-S" server)
     args)))

(defun sqlcmd-comint-start (&rest opts)
  (apply
   #'make-comint-in-buffer
   "sqlcmd"
   (get-buffer-create "*sqlcmd*")
   sqlcmd-program
   nil
   (apply #'sqlcmd-make-options opts)
   ))

(setq sqlcmd-queries
      (make-hash-table
       :dbname "SELECT DB_NAME()"))

(provide sqlcmd)
