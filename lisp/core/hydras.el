;;; -*- lexical-binding: t; -*-

(require 'hydra)

(with-eval-after-load 'evil-mc
  (defhydra evil-mc-hydra (:color pink
                           :pre (progn
                                  (evil-mc-pause-cursors)
                                  (evil-motion-state)
                                  (read-only-mode 1))
                           :post (progn
                                   (evil-normal-state)
                                   (evil-mc-resume-cursors)
                                   (read-only-mode -1)))
    ("." evil-mc-make-cursor-here "Cursor")
    ("M-n" evil-mc-make-and-goto-next-match "Next match")
    ("M-p" evil-mc-make-and-goto-prev-match "Prev match")
    ("C-n" evil-mc-make-cursor-move-next-line "Next line")
    ("C-p" evil-mc-make-cursor-move-prev-line "Prev line")
    ("*" evil-mc-make-all-cursors "Make all")
    ("u" evil-mc-undo-last-added-cursor "Undo")
    ("U" evil-mc-undo-all-cursors "Remove all")
    ("q" nil "quit")))

(provide 'core/hydras)
