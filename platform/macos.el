;;; -*- lexical-binding: t; -*-


(progn
  (setq mac-option-modifier 'meta)
  (setq window-resize-pixelwise t)
  (setq frame-resize-pixelwise t)
  (setq mac-command-modifier 'hyper)
  (setq inferior-lisp-program "sbcl"))
 
;; Generic Set of typical mac stuff
(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper f)] 'isearch-forward-regexp)
(global-set-key [(hyper w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(hyper z)] 'undo)
(global-set-key [(hyper ?`)] 'ns-next-frame)
(global-set-key [(hyper ?\;)] 'eval-expression)
(global-set-key [(hyper t)] 'tab-new)
(global-set-key [(hyper o)] 'other-window-prefix)


(provide 'macos)
