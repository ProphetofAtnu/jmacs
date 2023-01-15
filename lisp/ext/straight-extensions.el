;; -*- lexical-binding: t; -*-

(require 'f)
(require 'dash)

(defvar ext/*straight-used-package-list-file*
  (expand-file-name "var/ext/package-list.sexp" user-emacs-directory))

(defun ext//straight-used-package-file-exists ()
  (file-exists-p ext/*straight-used-package-list-file*))

(provide 'straight-extensions)
