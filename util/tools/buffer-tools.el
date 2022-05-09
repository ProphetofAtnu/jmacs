;; -*- lexical-binding: t; -*-

(defsubst buffer-line-string ()
  (buffer-substring
   (point-at-bol)
   (point-at-eol)))

(defsubst buffer-line-string-np ()
  (buffer-substring-no-properties
   (point-at-bol)
   (point-at-eol)))

(defun get-buffer-line (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (buffer-line-string)))

(defun get-buffer-line-np (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (buffer-line-string-np)))

(defun buffer-string-np ()
  "Same as buffer-string, without properties"
  (buffer-substring-no-properties
   (point-min) (point-max)))

(defmacro starting-at-point-min (&rest body)
  `(save-excursion
    (goto-char (point-min))
    ,@body))

(defmacro with-buffer-at-point-min (buffer &rest body)
  `(save-excursion
     (set-buffer (get-buffer ,buffer))
     (goto-char (point-min))
     ,@body))

(provide 'buffer-tools)
