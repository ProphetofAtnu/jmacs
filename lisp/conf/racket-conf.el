;;; -*- lexical-binding: t; -*-

(defvar js/racket-submit-timer nil)

(use-package racket-mode
    :straight t
    :general
    (local-leader-def
      :keymaps '(racket-mode-map)
      "e" 'racket-eval-last-sexp
      "s" 'racket-describe-search
      "," 'racket-repl
      )
    (local-leader-def
      :keymaps '(racket-repl-mode-map)
      "s" 'racket-describe-search
      "," 'racket-repl-switch-to-edit)
    :mode (("\\.rktl\\'" . racket-mode)
	   ("\\.rktd\\'" . racket-mode)
	   ("\\.rkt\\'" . racket-mode))
    :config
    (setq-mode-local racket-mode evil-lookup-func #'racket-xp-describe)
    ;; (setq-mode-local racket-repl-mode evil-lookup-func #'racket-repl-describe)
    (setq-mode-local racket-mode eldoc-documentation-function #'racket-xp-eldoc-function)
    ;; (setq-mode-local racket-repl-mode eldoc-documentation-function #'racket-repl-eldoc-function)
    (add-hook 'racket-mode-hook 'racket-xp-mode)
    (add-hook 'racket-mode-hook 'eldoc-mode)

    (defun js/racket-submit-advice (&rest ignored)
      (setf js/racket-submit-timer
	    (or js/racket-submit-timer
		(run-with-idle-timer 1 nil
		 (lambda ()
		   (racket--repl-refresh-namespace-symbols)
		   (setf
		    js/racket-submit-timer
		    nil)))))
      nil)
    (advice-add 'racket-repl-submit :after #'js/racket-submit-advice))

(provide 'racket-conf)
