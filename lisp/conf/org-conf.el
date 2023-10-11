;;; -*- lexical-binding: t; -*-

(require 'use-package)
(require 'general)

(use-package org-ext
    :general
  (:keymaps '(org-mode-map)
            :states '(normal)
            "RET" 'org-ext-dynamic-ret))

(use-package org
    :defer t
    :commands (org-agenda
               org-capture)
    :init
    (defvar org-directory "~/org")
    (define-prefix-map org-table)
    (define-prefix-map org-babel)
    :general
    (local-leader-def
        :keymaps '(org-mode-map)
      "," 'consult-org-heading
      "|" 'org-table-create-or-convert-from-region
      "y" 'org-store-link
      "i" 'org-insert-structure-template
      "s" 'org-sort
      "S" 'org-sort-entries
      "p" 'org-insert-last-stored-link
      "r" 'org-redisplay-inline-images
      "R" 'org-toggle-inline-images
      "c" '(:ignore t :wk "Crypto")
      "c e" 'org-encrypt-entry
      "c d" 'org-decrypt-entry
      "z" 'org-add-note
      "n" 'org-narrow-to-element
      "'" 'org-edit-special
      "t" (mount-prefix-map org-table "Table")
      "b" (mount-prefix-map org-babel "Babel")
      )
    (general-defs
        :keymaps 'prefix-org-table-map
      "c" 'org-table-insert-column
      "C" 'org-table-delete-column
      "r" 'org-table-insert-row
      "a" 'org-table-align
      "h" 'org-table-insert-hline
      "t" 'org-table-create-or-convert-from-region)
    :config
    (require 'org-ext)
    (setq org-src-preserve-indentation t)
    (setq org-confirm-babel-evaluate nil)
    (setq org-display-remote-inline-images 'download)
    (setq org-startup-with-inline-images t)
    (setq auto-save-default nil)
    (defun js/org-emphasize-headline-enable-explicit ()
      (org-headline-emphasize-minor-mode +1))
    (org-babel-do-load-languages 'shell t)
    (add-hook 'org-mode-hook (lambda ()
                               (auto-save-mode -1)))
    (add-hook 'org-mode-hook #'js/org-emphasize-headline-enable-explicit)
    (add-hook 'org-mode-hook 'org-display-inline-images))


(use-package org-download
    :straight t
    :hook (org-mode . org-download-enable)
    :general
    (local-leader-def
      :keymaps 'org-mode-map
      :major-modes t
      "d" '(:ignore t :wk "Download")
      "d c" 'org-download-clipboard
      "d s" 'org-download-screenshot)
    :config
    (setq org-download-method 'attach))

(use-package evil-org
    :straight t
    :hook (org-mode . evil-org-mode))

(use-package deft
  :straight t
  :commands (deft)
  :general
  (global-leader-def
      :keymaps 'override
      "n" 'deft
      "N" 'deft-new-file)
  :config
  (add-to-list 'evil-emacs-state-modes 'deft-mode)
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t)
  (setq deft-default-extension "org")
  (setq deft-use-filter-string-for-filename t
        deft-auto-save-interval 0)
  (setq deft-file-naming-rules
        '((nospace . "-")
          (case-fn . downcase)))
  (setq deft-directory
        (expand-file-name "deft" org-directory)))
  
(use-package ob
  :after (org)
  :general
  (general-defs
    :keymaps 'prefix-org-babel-map
    "I"	'org-babel-view-src-block-info
    "a"	'org-babel-sha1-hash
    "b"	'org-babel-execute-buffer
    "c"	'org-babel-check-src-block
    "d"	'org-babel-demarcate-block
    "e"	'org-babel-execute-maybe
    "f"	'org-babel-tangle-file
    "g"	'org-babel-goto-named-src-block
    "i"	'org-babel-lob-ingest
    "j"	'org-babel-insert-header-arg
    "k"	'org-babel-remove-result-one-or-many
    "l"	'org-babel-load-in-session
    "n"	'org-babel-next-src-block
    "o"	'org-babel-open-src-block-result
    "p"	'org-babel-previous-src-block
    "r"	'org-babel-goto-named-result
    "s"	'org-babel-execute-subtree
    "t"	'org-babel-tangle
    "u"	'org-babel-goto-src-block-head
    "v"	'org-babel-expand-src-block
    "x"	'org-babel-do-key-sequence-in-edit-buffer
    "z"	'org-babel-switch-to-session-with-code
    "h"	'org-babel-mark-block
    ))

(provide 'org-conf)
