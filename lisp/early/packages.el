;; -*- lexical-binding: t; -*-

(straight-use-package 'dash)
(straight-use-package 'm-buffer)
(straight-use-package 'f)
(straight-use-package 'ts)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'datetime)
(straight-use-package 'deferred)
(straight-use-package 'async)
(straight-use-package 'emacsql)
(straight-use-package 'emacsql-sqlite-builtin)
(straight-use-package 'zmq)
(straight-use-package 'esup)

(use-local! org-ext "util" ("org-ext.el"))
(use-local! proc-ext "util" ("proc-ext.el"))
(use-local! js-tools "util/tools")
(use-local! buflog "lisp/packages/buflog")

(provide 'early/packages)
