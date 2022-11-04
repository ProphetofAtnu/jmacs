;; -*- lexical-binding: t; -*-


(use-package tuareg-mode
  :straight t)

(use-package caml-mode
  :straight t)

(use-package merlin
  :straight t
  :config
  (add-hook 'merlin-mode-hook (lambda ()
				(setq-local evil-lookup-func
                                            #'merlin-document))))

(use-package utop
  :straight t
  :config
  (add-hook 'utop-mode-hook 'company-mode))


(provide 'ocaml-conf)
