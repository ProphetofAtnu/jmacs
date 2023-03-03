;; -*- lexical-binding: t; -*-

(defun ext/vterm-with-program (prg)
  (let ((vterm-shell (executable-find prg)))
    (when vterm-shell
      (vterm))))

(provide 'vterm-extensions)
