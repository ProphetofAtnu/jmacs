;; -*- lexical-binding: t; -*-

(defvar jq-exec-path "jq")

(defun jq--kw-flag (kw)
  (let ((name (string-trim-left (symbol-name kw) ":")))
    (if (length> name 1)
        (concat "--" name)
      (concat "-" name))))

(defun jq--fmt-command (filt &optional flags files)
  (flatten-tree
   (list
    jq-exec-path
    flags
    (format "'%s'" filt)
    files)))

(defun jq--build-command (filt &rest flags-and-files)
  (let ((flgs (mapcar #'jq--kw-flag (seq-filter #'keywordp flags-and-files)))
        (files (seq-remove #'keywordp flags-and-files)))
    (jq--fmt-command filt flgs files)))

(provide 'emacs-jq)
