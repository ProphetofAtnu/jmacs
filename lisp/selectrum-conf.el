;;; -*- lexical-binding: t; -*-
(require 'general)
(require 'use-package)

;; (use-package selectrum
;;     :straight t
;;     :hook (emacs-startup . selectrum-mode))

;; (use-package selectrum-prescient
;;   :straight t
;;   :hook (selectrum-mode . selectrum-prescient-mode))

(defvar js/vertico-multiform-setup-hook nil)

(defun js/vertico--reset-hooks ()
  "Debug function to reset the vertico hook"
  (setq js/vertico-multiform-setup-hook nil))

(defun js/vertico-get-category ()
  "Helper function for vertico multiform hooks to get the command category"
  (completion-metadata-get
   (completion-metadata
    (buffer-substring (minibuffer-prompt-end)
                      (max (minibuffer-prompt-end) (point)))
    minibuffer-completion-table
    minibuffer-completion-predicate)
   'category))

(defun js/vertico-multiform-hook-extension (&rest _)
  (run-hooks 'js/vertico-multiform-setup-hook))

(defun js/vertico-enable-extended-hook () 
  (advice-add 'vertico-multiform--setup :after #'js/vertico-multiform-hook-extension))

(defun js/vertico-disable-extended-hook ()
  (advice-remove 'vertico-multiform--setup #'js/vertico-multiform-hook-extension))

(defun js/vertico-file-extensions ())

;;; Enable vertico
(use-package vertico
    :straight t
    :init
    (add-to-list 'load-path
                 (straight--build-dir "vertico" "extensions"))
    (vertico-mode)
    (require 'vertico-multiform)
    (vertico-multiform-mode)
    (js/vertico-enable-extended-hook)
    (setq vertico-cycle t)
    (require 'vertico-flat)
    (require 'vertico-grid)
    (setq vertico-multiform-categories
          '((file grid)
            (command flat)))
    (require 'vertico-directory)

    (general-defs
        :keymaps 'vertico-map
      "C-M-o" 'vertico-multiform-flat
      "C-M-i" 'vertico-multiform-grid
      "C-M-u" 'vertico-multiform-vertical
      "C-." 'vertico-multiform-vertical
      )
    (general-defs
        :keymaps 'vertico-grid-map
      "M-n" 'vertico-grid-right
      "M-p" 'vertico-grid-left))

(use-package prescient
    :straight t)
(use-package company-prescient
    :straight t
    :hook (company-mode . company-prescient-mode))

(use-package orderless
    :straight t
  :init
 (setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
;; A few more useful configurations...
(use-package emacs
    :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (ido-mode -1)
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


(use-package consult
    :straight t
    :general
    (prefix-file-map
     "r" 'consult-recent-file
     "F" 'consult-find
     "l" 'consult-locate
     "b" 'consult-bookmark)
    (prefix-search-map
     "s" 'consult-line
     "r" 'consult-ripgrep
     "h" 'consult-outline
     "f" 'consult-focus-lines
     "m" 'consult-multi-occur
     "g" 'consult-grep)
    (prefix-help-map
     "a" 'consult-apropos
     "m" 'consult-man)
    (prefix-jump-map
     "m" 'consult-mark
     "M" 'consult-global-mark
     "b" 'consult-bookmark)
    (prefix-buffer-map
     "s" 'consult-line-multi
     "b" 'consult-buffer)
    (general-defs :keymaps '(override)
      "C-M-'" 'consult-imenu)
    :config
    (setq
     consult-fontify-preserve nil
     xref-show-xrefs-function 'consult-xref
     consult-preview-key (kbd "M-.")))

(use-package consult-projectile
    :straight t 
    :general
    (prefix-project-map
     "v" 'consult-projectile))

(use-package consult-dir
    :straight t
    :general
    (prefix-file-map
     "d" 'consult-dir))

(use-package embark
    :straight t
    :general
    (selectrum-minibuffer-map
     "C-SPC" 'embark-act)
    (vertico-map
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

  ;; :after (selectrum)
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
