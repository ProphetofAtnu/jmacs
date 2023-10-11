;;; -*- lexical-binding: t; -*-

(require 'general)
(require 'use-package)

(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package eat
  :commands (eat-eshell-mode eat)
  :general
  (:keymaps '(eat-mode-map)
            "M-DEL" 'eat-self-input)
  :init
  (defvar eat-sixel-aspect-ratio 1.0)
  (with-eval-after-load "eshell"
    (eat-eshell-mode +1))
  :config
  ;; (setq eat-default-cursor-type '(bar nil nil))
  )

(use-package shell-pop
  :straight t
  :general
  (override
   "C-'" 'shell-pop)
  :config
  (setq shell-pop-full-span t))

(use-package eshell
  :init
  ;; (add-hook 'eshell-load-hook #'eat-eshell-mode)
  )

(use-package pcomplete-extension
  :straight t)

(use-package bash-completion
  :straight t
  :commands (bash-completion-dynamic-complete-nocomint)
  :init
  (add-hook 'shell-dynamic-complete-functions #'bash-completion-dynamic-complete))

(use-package vterm
  :straight t
  :commands (vterm vterm-other-window)
  :general
  ('prefix-utility-map
   "v" 'vterm
   "V" 'vterm-other-window)
  :config
  (setq vterm-timer-delay 0))

(defun shell-scratch-create ()
  (interactive)
  (let ((sh-buf (get-buffer-create (generate-new-buffer "*shell-scratch*"))))
    (with-current-buffer sh-buf
      (sh-mode))
    (pop-to-buffer sh-buf)))

(use-package bash-comp-capf
  :hook (sh-mode-hook . bash-comp-capf-setup))

(with-eval-after-load 'vterm
  (defun vterm-send-region-to-shell (beg end)
    (interactive "r")
    (let* ((sub (buffer-substring-no-properties beg end))
           (vterm (match-buffers '(major-mode . vterm-mode)))
           (vterm-buf (car vterm)))
      (with-current-buffer vterm-buf
        (vterm-send-string (if (string-suffix-p "\n" sub)
                               sub
                             (concat sub "\n"))
                           t))))

  (defun vterm-send-line-to-shell (beg end)
    (interactive "r")
    (vterm-send-region-to-shell (point-at-bol) (point-at-eol)))

  (evil-define-operator js/evil-vterm-delete-forward-char (beg end type register)
    "Delete previous character."
    :motion evil-forward-char
    (interactive "<R><x>")
    (evil-collection-vterm-delete beg end type register))
  
  (local-leader-def
    :keymaps '(sh-mode-map)
    "v" '(:ignore t :wk "Vterm")
    "v r" 'vterm-send-region-to-shell
    "v l" 'vterm-send-line-to-shell
    )

  (general-defs
    :keymaps 'vterm-mode-map
    :states '(normal visual)
    "x" 'js/evil-vterm-delete-forward-char)
  )

;; (use-package elvish-mode
;;   :straight t
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                '(elvish-mode "elvish" "-lsp"))
;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-stdio-connection '("elvish" "-lsp"))
;;     :activation-fn (lsp-activate-on "elvish")
;;     :server-id 'elvish-lsp))
;;   (add-to-list 'lsp-language-id-configuration
;;                '(elvish-mode . "elvish")))

(use-package vterm-toggle
  :straight t
  :commands (vterm-toggle)
  :general
  ('prefix-utility-map
   "d" 'vterm-toggle-insert-cd
   "t" 'vterm-toggle))

(use-package mistty
  :straight t
  :commands (mistty))

(provide 'shell-conf)

