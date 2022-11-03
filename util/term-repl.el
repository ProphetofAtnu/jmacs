;; -*- lexical-binding: t; -*-

(require 'term)
;; (defgroup term-repl nil
;;   "Emacs term.el based repls, based on the current language.")

(defun term-repl-find-sbt-root ()
  (let ((path (split-string default-directory "/" t))
        cpath)
    (cl-loop for x from (length path) downto 0
          do (setf cpath (format "/%s" (string-join (seq-subseq path 0 x) "/")))
          when (member "build.sbt"(directory-files cpath))
          do (cl-return cpath))))

(defun term-repl-run-sbt-console ()
  (let ((dir (term-repl-find-sbt-root)))
    (if dir
        (let ((default-directory dir))
          (with-current-buffer
              (make-term "sbt-console" "sbt" nil "console")
            (term-char-mode)
            (current-buffer)))
      (with-current-buffer
          (make-term "scala" "scala")
        (term-char-mode)
        (current-buffer)))))

(defvar term-repl-language-alist
  '((scala-mode . term-repl-run-sbt-console)
    (python-mode . "ipython")
    (sh-mode . "bash")))

(defun term-repl--get (&optional buffer)
  (let* ((buf (or buffer (current-buffer)))
         (prg (alist-get (buffer-local-value 'major-mode buf) term-repl-language-alist)))
    (when (null prg)
      (error "Major mode %S has no repl defined" (buffer-local-value 'major-mode buf)))
    (if (symbol-function prg)
        (funcall (symbol-function prg))
      (make-term prg prg))))

(defun term-repl-run (&optional buffer)
  (interactive)
  (pop-to-buffer (term-repl--get buffer)))

(defun term-repl-send-string (expr &optional from-buffer)
  (let* ((target (term-repl--get from-buffer))
         (proc (get-buffer-process target)))
    (process-send-string proc expr)))

(defun term-repl-send-region (start end)
  (interactive "r")
  (let ((exp (buffer-substring-no-properties start end)))
    (term-repl-send-string exp (current-buffer))))

(defun term-repl-send-line (&optional at-point)
  (interactive "d")
  (save-excursion
    (goto-char (or at-point (point)))
    (let ((exp
           (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (term-repl-send-string exp (current-buffer)))))

(provide 'term-repl)
