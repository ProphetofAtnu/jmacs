;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)

(use-package helpful
  :straight t
  :commands (helpful-at-point)) 

(use-package macrostep
  :straight t
  :commands (macrostep-mode))

(use-package elisp-def
  :delight
  :straight t
  :hook ((emacs-lisp-mode . elisp-def-mode)
         (lisp-interaction-mode . elisp-def-mode)
         (ielm-mode . elisp-def-mode)))

(use-package eros
  :straight t
  :hook (emacs-lisp-mode . eros-mode))

(use-package ielm
  :commands (ielm)
  :config
  (setq ielm-dynamic-return nil)
  (add-hook
   'ielm-mode-hook
   (lambda ()
     (setq-local
      evil-lookup-func
      #'helpful-at-point))))


(use-package elisp-mode
  :commands (elisp-mode)
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (setq-local lisp-indent-function #'common-lisp-indent-function)))
  (add-hook 'emacs-lisp-mode-hook
            '(lambda ()
               (setq-local evil-lookup-func #'helpful-at-point)))
  (add-hook 'emacs-lisp-mode-hook
            '(lambda ()
               (let  ((auto-insert-query nil)
                      (auto-insert-alist
                       '((("\\.el\\'" . "Emacs Lisp header")
                          ""
                          ";;; -*- lexical-binding: t; -*-\n\n" '(setq lexical-binding t)))))
                 (auto-insert)))))

(local-leader-def
  :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
  "e" 'eval-last-sexp
  "d" 'eval-defun
  "r" 'eval-region
  "m" 'macrostep-mode)
  

(provide 'emacs-lisp-conf)
