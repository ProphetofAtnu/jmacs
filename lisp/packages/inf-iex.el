;;; inf-iex-compl.el ---                             -*- lexical-binding: t; -*-

(require 'dash)

(defvar inf-iex-prompt-rx
  "^iex([[:digit:]]+)> ")

(defvar inferior-iex-program "iex")

(defvar iex-redirect-buffer " *iex-completions*")

(defconst inf-iex--redir-finished (rx
                                   (group (1+ ?\b))
                                   (1+ space)
                                   (backref 1)))

(defvar-local *inf-iex--last-prefix* nil)
(defvar-local *inf-iex--last-result* nil)
(defvar-local *inf-iex--top-level-mods* nil)

(defvar-local *iex-proc* nil)

(defun inf-iex-collect-input ()
  (funcall comint-get-old-input))

(defun inf-iex--read-completions-buffer ()
  (when-let ((buf (get-buffer iex-redirect-buffer)))
    (with-current-buffer buf
      (save-excursion
        (let ((eofl (progn (goto-char (point-min))
                           (point-at-eol))))
          (goto-char (point-max))
          (split-string
           (buffer-substring-no-properties
            eofl
            (point-at-bol))
           "[[:space:]]" t))))))

(defun inf-iex--propertize-completion-item (item)
  (let* 
      ((items (split-string item "/"))
       (compl (car items))
       (md (cdr-safe items)))
    (propertize compl
                :arity (car md))))

(defun inf-iex--propertize-completions (comps)
  (mapcar #'inf-iex--propertize-completion-item comps))

(defun inf-iex--retrieve-completions ()
  (inf-iex--propertize-completions
   (inf-iex--read-completions-buffer)))

(defun inf-iex-get-native-completions (process stub)
  "Natively complete elixir statement `stub' by querying the IEx
process, `process'"
  (with-current-buffer (process-buffer process)
    (let* ((original-filter-fn (process-filter process))
           (redirect-buffer (get-buffer-create
                             iex-redirect-buffer))
           (trigger "\x9")
           (clear-repl "\x01\x0b")
           (current-input (if (string= "" stub)
                              " "
                            stub))
           (done nil))
      (with-current-buffer redirect-buffer
        (erase-buffer)
        (process-send-string process (concat current-input trigger clear-repl))
        (set-process-filter process
                            #'(lambda (p str)
                                (with-current-buffer redirect-buffer
                                  (save-excursion
                                    (goto-char (point-max))
                                    (insert str)
                                    (when (looking-back inf-iex--redir-finished)
                                      (setf done t)
                                      (set-process-filter p original-filter-fn))))))
        (while (not done)
          (accept-process-output process 1 0 t)
          )
        (inf-iex--retrieve-completions)))))

(defun inf-iex--get-completion-range ()
  (save-excursion
    (reverse (list (point)
                   (progn
                     (skip-syntax-backward "\w_")
                     (point))
                   (progn
                     (skip-syntax-backward
                      "w_.")
                     (point))))))

(defun inf-iex--complete-mods-tl (proc &rest ignored)
  (if *inf-iex--top-level-mods*
      *inf-iex--top-level-mods*
    (let ((res
           (inf-iex-get-native-completions proc "Elixir.")))
      (setq *inf-iex--top-level-mods* res)
      res)))

(defun inf-iex--annot-func (item)
  (if-let ((prop (get-text-property 0 :arity item)))
      (format "/%s" prop)
    " -> mod"))

(defun inf-iex--completion-function (proc prefix item &rest ignored)
  (if (and *inf-iex--last-result*
           (string= *inf-iex--last-prefix*
                    prefix))
      *inf-iex--last-result*
    (let ((res
           (inf-iex-get-native-completions proc prefix)))
      (setq *inf-iex--last-prefix* prefix)
      (setq *inf-iex--last-result* res)
      res)))

(defun inf-iex-completion-at-point ()
  "Completion at point function for inferior iex process"
  (-let* (((expr start end) (inf-iex--get-completion-range))
          (item (buffer-substring-no-properties start end))
          (prefix (buffer-substring-no-properties expr start))
          (base-ctable
           (completion-table-dynamic
            (apply-partially
             #'inf-iex--completion-function
             (get-buffer-process (current-buffer))
             prefix item))))
    (unless (syntax-ppss-context (syntax-ppss))
      (list start end
            (cond
              ((string= prefix "")
               (completion-table-merge
                base-ctable
                (completion-table-dynamic
                 (apply-partially #'inf-iex--complete-mods-tl
                                  (get-buffer-process (current-buffer))))))
              (t base-ctable))
            :annotation-function #'inf-iex--annot-func))))

(define-derived-mode inferior-iex-mode
    comint-mode "Inferior IEx"
    (add-hook 'completion-at-point-functions
              #'inf-iex-completion-at-point nil 'local))

(cl-defun create-iex-buffer (&key
                               (buffer "*iex*")
                               (exec "iex")
                               (proc-name "iex")
                               (args nil))
  (let ((comint-terminfo-terminal "vt100"))
    (with-current-buffer (apply #'make-comint-in-buffer
                                proc-name buffer exec nil args)
      (inferior-iex-mode)
      (setq *iex-proc*
            (get-buffer-process
             (current-buffer)))
      (current-buffer))))

(defun run-iex (&rest args)
  (interactive)
  (display-buffer
   (create-iex-buffer
    :exec inferior-iex-program)))

(defun run-iex-with-mix ()
  (interactive)
  (display-buffer
   (create-iex-buffer
    :buffer "*iex-mix*"
    :proc-name "iex-mix"
    :exec inferior-iex-program
    :args '("-S" "mix"))))

(defun inf-iex--get-help (proc str)
  (process-send-string
   proc
   (format "h %s\n" str)))

(provide 'inf-iex)
