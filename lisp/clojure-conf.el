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

(use-package a
    :straight t)

(with-eval-after-load 'le-clojure
  (setf (symbol-function 'lispy--clojure-middleware-load) (lambda () nil)))

(setq vc-follow-symlinks t)

(use-package cider
    :straight t
    :after (clojure-mode)
    :general
    (local-leader-def
        :keymaps '(cider-mode-map cider-clojure-interaction-mode-map)
      "," 'cider
      "C-c" 'cider-interrupt
      "c" 'cider-find-and-clear-repl-output
      "x" 'cider-run
      "i" 'cider-inspect-expr
      "e" 'cider-eval-defun-at-point
      "b" 'cider-eval-buffer
      "r" 'cider-eval-region
      "N" 'cider-eval-ns-form
      "Q" 'cider-restart
      "q" 'cider-ns-refresh
      "w" 'cider-repl-set-ns
      "a" 'cider-apropos
      "s" 'cider-scratch
      "S" 'cider-scratch-reset
      "n" 'cider-browse-ns
      "p" 'cider-load-all-project-ns
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

(use-package spiral
    :straight t
    :after (clojure-mode)
    :general
    (local-leader-def
        :keymaps '(spiral-mode-map)
      "e" 'spiral-eval-top-level-form
      "l" 'spiral-eval-last-sexp)
    :config
    (add-hook 'spiral-repl-mode-hook 'company-mode)
    (require 'cider-completion)
    (defalias 'spiral-complete--get-context-at-point
        'cider-completion-get-context-at-point)
    (defvar spiral-aux-sync-request-timeout spiral-sync-request-timeout))

(provide 'clojure-conf)
