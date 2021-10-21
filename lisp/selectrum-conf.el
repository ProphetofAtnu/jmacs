;;; -*- lexical-binding: t; -*-

(require 'general)
(require 'use-package)

(use-package selectrum
  :straight t
  :hook (emacs-startup . selectrum-mode))

(use-package selectrum-prescient
  :straight t
  :hook (selectrum-mode . selectrum-prescient-mode))

(use-package consult
    :straight t
    :general
    (prefix-file-map
     "r" 'consult-recent-file
     "F" 'consult-find
     "b" 'consult-bookmark)
    (prefix-search-map
     "s" 'consult-line
     "r" 'consult-ripgrep
     "m" 'consult-multi-occur
     "g" 'consult-grep)
    (prefix-help-map
     "a" 'consult-apropos)
    (prefix-buffer-map
     "b" 'consult-buffer)
    :config
    (setq
     consult-fontify-preserve nil
     xref-show-xrefs-function 'consult-xref
     consult-preview-key (kbd "M-.")))

(use-package embark
  :straight t
  :after (selectrum)
  :general
  (selectrum-minibuffer-map
   "C-SPC" 'embark-act))


(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package marginalia
  :straight t
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  ;; :bind (("M-A" . marginalia-cycle)
  ;;        :map minibuffer-local-map
  ;;        ("M-A" . marginalia-cycle))

  :general (:keymaps 'minibuffer-local-map
            "M-?" 'marginalia-cycle)

  :after (selectrum)
  ;; The :init configuration is always executed (Not lazy!)
  :init
  (marginalia-mode)

  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit 'keep-selected))))

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle' to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))


(provide 'selectrum-conf)
