;;; -*- lexical-binding: t; -*-


(progn
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'hyper))

;; Generic Set of typical mac stuff
(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(hyper z)] 'undo)


(provide 'macos)
