;; -*- lexical-binding: t; -*-

(straight-use-package 'tuareg-mode)
(straight-use-package 'caml-mode)

(use-package dune
  :straight t)

(use-package merlin
  :straight t
  :hook (tuareg-mode . merlin-mode)
  :general
  (local-leader-def 'tuareg-mode-map
    "i" 'merlin-switch-to-ml
    "I" 'merlin-switch-to-mli)
  :config
  (add-hook 'merlin-mode-hook (lambda ()
				(setq-local evil-lookup-func
                                            #'merlin-document))))

(use-package utop
  :straight t
  :config
  (add-hook 'utop-mode-hook 'company-mode))


(provide 'ocaml-conf)
