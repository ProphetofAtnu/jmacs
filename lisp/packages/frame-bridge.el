;;; -*- lexical-binding: t; -*-

(require 'posframe)

(defun get-shown-windows ()
  (let ((w nil))
    (dolist (f (frame-list) w)
      (setf w (append w (window-list f))))))

(let* 
    ((b (get-buffer-create " *test-frame*"))
     (f (posframe-show b 
                       :border-width 1
                       :border-color "gray"
                       :width 30
                       :accept-focus t
                       :position (point))))
  (with-current-buffer b
    (general-define-key
     :states 'motion
     :keymaps 'local
     "q" 'kill-this-buffer)
    (evil-motion-state +1)
    (insert "Test"))
  (select-frame-set-input-focus f))

(provide 'frame-bridge)
