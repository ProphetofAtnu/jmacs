;; -*- lexical-binding: t; -*-

(use-package which-key
  :straight t
  :delight
  :hook (emacs-startup . which-key-mode)
  :config
  (add-to-list 'which-key-replacement-alist
               '((nil . "evil-collection-\\(.*\\)") . (nil . "ec-\\1"))))

(use-package which-key-posframe
  :straight t
  :delight
  :hook (which-key-mode . which-key-posframe-mode)
  :config
  (setq which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-center))

(provide 'global/global-which-key)
