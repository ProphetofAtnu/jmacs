;; -*- lexical-binding: t; -*-

(require 'switch-window)

(defun rehome-window ()
  "Interactively change the location of the current window,
deleting it's old position."
  (interactive)
  (let ((cwin (selected-window))
        (cfr (selected-frame))
        (cb (current-buffer))
        (owcfg (current-window-configuration))
        (confirmed nil))
    (unwind-protect
        (progn (delete-window)
               (switch-window)
               (cl-case (read-char-choice-with-read-key "Direction [hkklr]: "
                                                        '(?h ?j ?k ?l ?r) nil)
                 (?h (progn (split-window-right)
                            (display-buffer-same-window cb '())))
                 (?l (progn (split-window-right)
                            (select-window (window-in-direction 'right))
                            (display-buffer-same-window cb '())))
                 (?k (progn (split-window-below)
                            (display-buffer-same-window cb '())))
                 (?j (progn (split-window-below)
                            (select-window (window-in-direction 'below))
                            (display-buffer-same-window cb '())))
                 (?r (display-buffer-same-window cb '())))
               (setf confirmed t))
      (unless confirmed
        (set-window-configuration owcfg nil nil)
        (set-frame-selected-window cfr cwin)
        (select-frame-set-input-focus cfr)))
    ))

(provide 'window-tools)
