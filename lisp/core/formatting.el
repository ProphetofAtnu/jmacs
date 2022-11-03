;;; -*- lexical-binding: t; -*-


(defvar-local format-action nil)

(defun format-this-buffer ()
  (interactive)
  (if (functionp format-action)
      (funcall-interactively format-action nil)
    (message
     "No formatting command defined for this buffer. Set it using (setq-local format-action [fun])")))  
  


(provide 'core/formatting)
