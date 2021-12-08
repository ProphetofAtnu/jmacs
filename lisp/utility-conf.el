;;; -*- lexical-binding: t; -*-

(require 'core/utility)

(create-open-functions dired-hexl-open (file)
                       (interactive
                        (list (dired-file-name-at-point)))
                       (with-current-buffer 
                           (find-file-noselect file)
                         (hexl-mode 1)
                         (current-buffer)))

(use-package ranger
    :straight t
    :general
    (:keymaps '(ranger-mode-map)
              "w x" 'dired-hexl-open-other-window
              "w X" 'dired-hexl-open-other-window-noselect))

(provide 'utility-conf)