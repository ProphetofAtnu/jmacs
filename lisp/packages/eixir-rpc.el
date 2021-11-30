;;; -*- lexical-binding: t; -*-


(defvar elixir-iex-prompt-regex "^iex([0-9])>")

(defvar elixir-cmd (executable-find "elixir"))
(defvar iex-cmd (executable-find "iex"))

(defvar elixir-rpc-buffer-name "*elixir-rpc*")
(defvar elixir-rpc-proc-name "elixir-rpc")
(defvar *elixir-rpc-node-target* nil)
(defvar elixir-rpc--completion-table '())

(defvar elixir-rpc--last-complete '(0 0 0 0))

(defvar-local elixir-rpc--last-prefix "")

(defvar elixir-rpc-debounce-time 5)

(defun elixir-rpc-args-builder (node command)
  (list
   "--sname"
   "contr"
   "--rpc-eval"
   "master@localhost"
   command))

(defun elixir-rpc-get-module-alias (atom)
  (string-trim-right atom "\\.?[^\\.]*"))

(defun elixir-rpc-same-module-alias (partial)
  (let ((mod (elixir-rpc-get-module-alias partial)))
    (lambda (x)
      (if
          (equal
           mod
           (elixir-rpc-get-module-alias x))
          (progn 
            x)
        nil))))

(defun elixir-rpc-call (call &optional node call-in-buffer) 
  (let ((node (or node
                  *elixir-rpc-node-target*))
        (buffer (get-buffer-create elixir-rpc-buffer-name)))
    (with-current-buffer buffer
      (when (functionp call-in-buffer)
        (funcall call-in-buffer))
      (erase-buffer))
    (make-process
     :name elixir-rpc-proc-name
     :buffer buffer
     :sentinel #'ignore
     :command (cons 
               elixir-cmd
               (elixir-rpc-args-builder
                node
                call)
               ))))

(defun elixir-rpc-call-expression (call &optional node)
  (elixir-rpc-call
   (format "%s |> IO.inspect(limit: :infinity)" call)
   node))

(defun elixir-rpc--completions-should-run (partial)
  (with-current-buffer
      (get-buffer-create
       elixir-rpc-buffer-name)
    (when (or (> (time-to-seconds
                     (time-since
                      elixir-rpc--last-complete))
                    elixir-rpc-debounce-time)
              (not (equal (elixir-rpc-get-module-alias
                           partial)
                          (elixir-rpc-get-module-alias
                           elixir-rpc--last-prefix))))
      (progn
        (setq elixir-rpc--last-prefix
              partial)
        (setq elixir-rpc--last-complete
              (current-time))
        t))))

(defun elixir-rpc-call-completions (partial &optional node)
  (let ((arg (string-reverse partial)))
      (elixir-rpc-call
       (format
        "IEx.Autocomplete.expand('%s') |> elem(2) |> IO.inspect(limit: :infinity)"
        arg)
       node)))

(defun elixir-rpc--parse-completions ()
    (with-current-buffer (get-buffer elixir-rpc-buffer-name)
      (goto-char (point-min))
      (let ((m nil)
            (prefix
             (string-trim-right elixir-rpc--last-prefix "[^\\.]+$")))
        (condition-case nil
            (while t
              (progn 
                (re-search-forward "'\\([^']+\\)'")
                (push (concat prefix (match-string 1)) m)))
          (error m)))))

(defun elixir-rpc--read-completions (&optional no-wait)
  (unless no-wait
    (accept-process-output (get-process elixir-rpc-proc-name)))
  (let ((r (elixir-rpc--parse-completions)))
    (mapc #'(lambda (x) (add-to-list 'elixir-rpc--completion-table
                                x))
          r)
    r))

(defun elixir-rpc-get-completions (partial &optional node)
  (elixir-rpc-call-completions partial node)
  (elixir-rpc--read-completions))

(defun elixir-rpc--complete-for-capf (s)
  (print s)
  (mapcar (lambda (s) (string-trim-right s "/[0-9]+")) (elixir-rpc-get-completions "")))

(defun elixir-rpc-completion-at-point-comint ()
  (let ((start (save-excursion (comint-bol-or-process-mark)))
        (end (point)))
    (list 
     start end
     (completion-table-dynamic 
      #'elixir-rpc--complete-for-capf))
    ))

(provide elixir-rpc)
