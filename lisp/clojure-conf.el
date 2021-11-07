;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)

(use-package clojure-mode
  :straight t
  :defer t
  :config
  (progn 
    (require 'core/utility)
    (use-local-pairs 'clojure-mode-hook
                     '((?{ . ?})))))

(use-package cider
    :straight t
    :after (clojure-mode)
    :general
    (local-leader-def
        :keymaps '(clojure-mode-map cider-clojure-interaction-mode-map)
      "," 'cider
      "C-c" 'cider-interrupt
      "x" 'cider-run
      "e" 'cider-eval-defun-at-point
      "b" 'cider-eval-buffer
      "r" 'cider-eval-region
      "Q" 'cider-restart
      "q" 'cider-ns-refresh
      "w" 'cider-repl-set-ns
      "a" 'cider-apropos
      "s" 'cider-scratch
      "S" 'cider-scratch-reset
      "n" 'cider-browse-ns
      "=" 'cider-format-buffer
      "u" 'cider-undef
      "?" 'cider-classpath
      "z" 'cider-load-buffer-and-switch-to-repl-buffer
      "l" 'cider-inspect-last-sexp)
    (:keymaps 'cider-browse-ns-mode-map
     :states '(normal motion visual)    
     "q" 'cider-popup-buffer-quit-function
     "RET" 'cider-browse-ns-operate-at-point    
     "<return>" 'cider-browse-ns-operate-at-point)    
    :config
    (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-repl-mode-hook 'company-mode))

(provide 'clojure-conf)
