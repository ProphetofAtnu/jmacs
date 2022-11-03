;; -*- lexical-binding: t; -*-
;; Local Variables:
;; read-symbol-shorthands: (("isp" . "ishell-parse--"))
;; End:

(defvar ishell-syntax-table
  (let ((tbl (make-syntax-table))
        (punc '(?| ?< ?> ?| ?& ?$))
        (normal-symparts '(?_ ?-))
        (paro '(?\( ?\[ ?{))
        (parc '(?\) ?\] ?\}))
        (quotes '(?\" ?\' ?\`))
        (escape '(?\\)))
    (dolist (c punc)
      (modify-syntax-entry c "." tbl))
    (dolist (c normal-symparts)
      (modify-syntax-entry c "_" tbl))
    (dolist (c paro)
      (modify-syntax-entry c "(" tbl))
    (dolist (c parc)
      (modify-syntax-entry c ")" tbl))
    (dolist (c quotes)
      (modify-syntax-entry c "\"" tbl))
    tbl))

(provide 'ishell-parse)

