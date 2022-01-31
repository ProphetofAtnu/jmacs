;;; -*- lexical-binding: t; -*-

(defun inf-iex-mfa-autocomplete (prefix)
  (let ((arg (reverse prefix)))
    (format "Elixir.IEx.Autocomplete expand [\"%s\"]" arg)))

(defvar inf-iex-buffer-name " *erl_call*")
(defvar inf-iex-repl-buffer-name "*iex*")
(defvar inf-iex-process-name "iex")

(defvar inf-iex--compl-proc nil)
(defvar inf-iex-last-arg nil)
(defvar inf-iex-current-arg nil)
(defvar-local inf-iex-is-mix nil)

(defvar inf-iex-stubs '())
(defvar inf-iex-resultrx "{\\(?:yes\\|no\\), [\"\[]\\(.*\\)[\]\"], \\[\\(.*\\)\\]}")

(defvar inf-iex--cached-completions (make-hash-table
                                     :test 'equal))

(defun inf-iex--detect-mix-project ()
  (cl-do ((mix-dir nil)
          (pth (reverse (split-string (expand-file-name ".") "/"))
               (cdr pth)))
         ((null pth) nil)
    (let ((dn (string-join (reverse pth) "/")))
      (when (member "mix.exs" (directory-files dn))
        (cl-return dn)))))

(define-derived-mode
    inf-iex-mode comint-mode "IEx"
    (add-hook 'completion-at-point-functions
              'inf-iex-completion-at-point 0 t))

(defun inf-iex-run ()
  (interactive)
  (with-current-buffer (get-buffer-create inf-iex-repl-buffer-name)
    (unless (process-live-p (get-buffer-process (current-buffer)))
      (inf-iex-mode)
      (make-comint-in-buffer inf-iex-process-name inf-iex-repl-buffer-name "iex" nil
                             "--sname" "iiex"))
    (display-buffer (current-buffer))))

(defun inf-iex-mix-run ()
  (interactive)
  (when-let ((d (inf-iex--detect-mix-project)))
    (with-current-buffer (get-buffer-create inf-iex-repl-buffer-name)
      (cd d)
      (unless (process-live-p (get-buffer-process (current-buffer)))
        (inf-iex-mode)
        (make-comint-in-buffer inf-iex-process-name inf-iex-repl-buffer-name "iex" nil
                               "--sname" "iiex" "-S" "mix")
        (setq-local inf-iex-is-mix t))
      (display-buffer (current-buffer)))))

(defun inf-iex-recompile ()
  (interactive)
  (comint-send-string (get-process inf-iex-process-name) "recompile()\n"))

(defun inf-iex--ac-filter-builder (stub)
  (lambda (proc dat)
    (with-current-buffer
        (process-buffer proc)
      (erase-buffer)
      (when (string-match
             inf-iex-resultrx
             dat)
        (when (length>
               (match-string 1 dat)
               0)
          (insert (concat stub (match-string 1 dat)))
          (newline))
        (let ((r "\"\\([^\",]+\\)\"")
              (cand (match-string 2 dat))
              (idx 0))
          (while (setf idx
                       (and (length> cand idx)
                            (string-match r cand idx)))
            (insert (match-string 1 cand))
            (newline)
            (cl-incf idx)))))))

(defun inf-iex--do-ac (arg &optional append)
  (with-current-buffer (get-buffer-create inf-iex-buffer-name)
    (unless (and inf-iex--compl-proc
                 (process-live-p inf-iex--compl-proc))
      (setq inf-iex--compl-proc
            (make-process
             :name "erl_call"
             :buffer inf-iex-buffer-name
             :filter  (inf-iex--ac-filter-builder (or append arg))
             :sentinel '(lambda (p s) nil)
             :command (list
                       "erl_call"
                       "-a"
                       (inf-iex-mfa-autocomplete arg)
                       "-sname"
                       "iiex@localhost")))))
  inf-iex--compl-proc)

(defun inf-iex--read-result () 
  (with-current-buffer (get-buffer inf-iex-buffer-name)
    (goto-char (point-min))
    (let ((lns (list
                (buffer-substring-no-properties
                 (point-at-bol)
                 (point-at-eol)))))
      (while (line-move 1 t)
        (let ((lcontent (buffer-substring-no-properties
                         (point-at-bol)
                         (point-at-eol))))
          (when (length> lcontent 0)
            (push lcontent
                  lns))))
      (nreverse lns))))

(defun inf-iex--to-capf-fmt (comps)
  (mapcar
   #'(lambda (x)
     (cl-destructuring-bind (sym . ar)
         (split-string x "/")
         (if-let ((arity (car-safe ar)))
             (propertize sym 'arity arity)
           sym)))
   comps))

(defun inf-iex--annot-func (x)
  (when-let
      ((prop
        (plist-get
         (text-properties-at
          0 x)
         'arity)))
    (format "/%s" prop)))

(defun inf-iex--point-region ()
    (save-excursion
    (let ((e (point))
          (ls (skip-syntax-backward "^.-"))
          (s (skip-syntax-backward "w_.")))
      (list (+ e ls s) (+ e ls) e))))

(defun inf-iex--extract-module (part)
    (string-trim-right part "\\.?[^\\.]*$"))

(defun inf-iex--cache-completions (query results)
  (let* ((mod (inf-iex--extract-module query))
         (cur (gethash mod inf-iex--cached-completions nil)))
    (dolist (x results)
      (unless (member x cur)
        (push x cur)))
    (puthash mod cur inf-iex--cached-completions)))

(defun inf-iex--get-cached-completions (st sep end)
  (lambda (_) 
    (let* ((ac (buffer-substring-no-properties st end))
           (mod (inf-iex--extract-module ac)))
      (gethash mod inf-iex--cached-completions nil))))

(defun inf-iex--get-completions (st sep end)
  (let* ((ac (buffer-substring-no-properties st end))
         ;; completion region
         (r (buffer-substring-no-properties sep end))
         (proc (inf-iex--do-ac ac (unless (string-empty-p r)
                                    r))))
    (lambda (prefix)
      (when (waiting-for-user-input-p)
        (accept-process-output proc nil 100 t))
      (let ((dat (inf-iex--read-result)))
        (when (length> dat 0)
          (let ((fmt (inf-iex--to-capf-fmt dat)))
            (run-with-idle-timer 1 nil 'inf-iex--cache-completions ac fmt)
            fmt))))))

(defun inf-iex-completion-at-point ()
  (cl-destructuring-bind (st sep end) (inf-iex--point-region)
    (list
     sep end
     (completion-table-merge
      (completion-table-dynamic (inf-iex--get-cached-completions st sep end))
      (completion-table-with-cache (inf-iex--get-completions st sep end)))
     :annotation-function 'inf-iex--annot-func)))

(provide 'inf-iex)
