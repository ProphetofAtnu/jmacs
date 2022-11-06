;;; -*- lexical-binding: t; -*-

(require 'core/utility)

(create-open-functions dired-hexl-open (file)
                       (interactive
                        (list (dired-file-name-at-point)))
                       (with-current-buffer 
                           (find-file-noselect file)
                         (hexl-mode 1)
                         (current-buffer)))

(use-package citre
    :straight t
    :defer t
    :init
    (require 'citre-config)
    (setq citre-enable-capf-integration nil))

(defun dired-down-dir ()
  (interactive)
  (when (directory-name-p
         (dired-file-name-at-point))
    (dired-open-file)))

(use-package dirvish
    :straight t
    :defer nil
    :config
    (general-defs
        :keymaps '(dirvish-mode-map)
      :states '(normal motion)
      "l" 'dired-down-dir
      "h" 'dired-up-directory
      "TAB" 'dirvish-subtree-toggle
      "s" 'dirvish-narrow
      "," 'dirvish-dispatch
      )
    (global-leader-def
        :keymaps 'override
      "t" 'dirvish-side)
    (dirvish-override-dired-mode 1))

(evil-define-text-object evil-entire-buffer (count &optional beg end type)
  (flatten-list (bounds-of-thing-at-point 'buffer)))

(general-defs
    :keymaps '(evil-inner-text-objects-map
               evil-outer-text-objects-map)
  "e" 'evil-entire-buffer)

(use-package dired
    :general
  (dired-mode-map
   "-" 'dired-up-directory))

(general-define-key
 :states '(normal motion)
 :keymaps 'Info-mode-map
 "o" 'ace-link
 )

(use-package pdf-tools
  :straight t)

(use-package elfeed
    :straight t)

(use-package plz
    :straight t)

(provide 'utility-conf)
