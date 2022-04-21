;; -*- lexical-binding: t; -*-

(require 'thunk)

(defvar-local apipy--unread-lines 0)
(defvar-local apipy--delay-timer nil)
(defvar-local apipy--remote-buffer nil)


(defvar-local apipy--backed-buffer-sanity-timer nil)
(defvar-local apipy--buffer-dirty nil)

(defvar apipy-ctr 0)
(defvar apipy--handlers (make-hash-table))

(defvar apipy--data-directory (if load-file-name
                                  ;; File is being loaded.
                                  (file-name-directory
                                   load-file-name)
                                ;; File is being evaluated using, for example, `eval-buffer'.
                                default-directory))

(defvar *apipy-kernel* nil)

(defvar *apipy-isolate-context* nil)

(defun apipy-ensure-kernel ()
  (or *apipy-kernel*
      (setq *apipy-kernel*
            (start-apipy-process))))

(defun apipy-reset-kernel-state ()
  (kill-process *apipy-kernel*)
  (setq *apipy-kernel* nil))

(defun apipy--trigger-flush-delayed (proc)
  (with-current-buffer (process-buffer proc)
    (unless (and
             apipy--delay-timer
             (timer--activate apipy--delay-timer))
      (setq-local apipy--delay-timer
                  (run-with-timer
                   0 nil
                   (lambda ()
                     (apipy--flush-messages proc)))))))

(defun apipy--process-filter (proc msg)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (let ((lines (cl-count ?\n msg)))
      (setq-local apipy--unread-lines
                  (+ apipy--unread-lines lines))
      (insert msg)
      (when (> apipy--unread-lines 0)
        (apipy--trigger-flush-delayed proc))))
  )

(defun start-apipy-process ()
  (make-process
   :name "apipython"
   :buffer (generate-new-buffer "*apipython*")
   :command (list "python3" (expand-file-name "run.py" apipy--data-directory))
   :filter #'apipy--process-filter))

(cl-defun apipy-build-message (method
                          &key args kwargs target tracked)
  (let ((msg (list
              (cons :method method)))
        (tid (when tracked
               (cl-incf apipy-ctr))))
    (when args
      (push (cons :args args) msg))
    (when kwargs
      (push (cons :kwargs kwargs) msg))
    (when target
      (push (cons :target target) msg))
    (when tid
      (push (cons :id tid) msg))
    (cons tid (concat (json-encode msg) "\n"))))

(defun apipy--read-message (proc)
  (with-current-buffer (process-buffer proc)
    (when (> apipy--unread-lines 0)
      (goto-char (point-min))
      (let ((pnt (point))
            (data (json-read)))
        (delete-region pnt (point))
        (cl-decf apipy--unread-lines)
        data))))

(defun apipy--add-tracked (id cb)
  (puthash id
           (lambda (result)
             (remhash id apipy--handlers)
             (funcall cb result))
           apipy--handlers))

(defun apipy--is-response (msg)
  (alist-get 'id msg))

(defun apipy--flush-messages (proc)
  (with-current-buffer
      (process-buffer proc)
    (let (msg)
      (while (setf msg (apipy--read-message proc))
       (if-let ((resid (apipy--is-response msg))
                (handler (gethash resid apipy--handlers nil)))
           
           (if (alist-get 'error msg)
               (message "PYAPI Error: %s" (alist-get 'error msg))
             (funcall handler (alist-get 'result msg))))))))

(cl-defun apipy-send-async-broadcast (proc method
                            &key args kwargs target)
  (let ((msg (apipy-build-message
              method :args args :kwargs kwargs :target target)))
    (process-send-string proc (cdr msg))))

(cl-defun apipy-send-async (proc cb method
                            &key args kwargs target)
  (let ((msg (apipy-build-message
              method :args args :kwargs kwargs :target target
              :tracked t)))
    (apipy--add-tracked (car msg) cb)
    (process-send-string proc (cdr msg))))

(cl-defun apipy-send-sync (proc method
                            &key args kwargs target)
  (let ((msg (apipy-build-message
              method :args args :kwargs kwargs :target target
              :tracked t))
        done
        cbv)
    (apipy--add-tracked (car msg)
                        (lambda (result)
                          (setf cbv result)
                          (setf done t)))
    (process-send-string proc (cdr msg))
    (while (not done)
      (accept-process-output proc 0 100 t))
    cbv))

(cl-defun apipy-send-thunk (proc method
                            &key args kwargs target transform)
  (let ((msg (apipy-build-message
              method :args args :kwargs kwargs :target target
              :tracked t))
        done
        cbv)
    (apipy--add-tracked (car msg)
                        (lambda (result)
                          (setf cbv
                                (if (functionp transform)
                                    (funcall transform result)
                                  result))
                          (setf done t)))
    (process-send-string proc (cdr msg))
    (thunk-delay 
     (while (not done)
       (accept-process-output proc 0 100 t))
     cbv)))

(cl-defstruct apipy-remote-buffer
  proc
  ref)

(defun apipy-new-buffer (proc)
  (let ((self (apipy-send-sync
               proc "eval/create_buffer")))
    (make-apipy-remote-buffer :proc proc :ref self)))

(defun apipy-delete-buffer (buf)
  (apipy-send-sync
   (apipy-remote-buffer-proc buf) "ref/remove_ref"
   :args (list (apipy-remote-buffer-ref buf))))

(defun apipy-remote-buffer-sync (py bufn &optional loud)
  (with-slots (proc ref) py
    (apipy-send-async
     proc
     (lambda (_)
       (with-current-buffer bufn
         (setq-local apipy--buffer-dirty nil))
       (when loud
         (message
          "Updated buffer %s"
          bufn)))
     "sync"
     :target ref
     :args (list
            (with-current-buffer bufn
              (buffer-substring-no-properties
               (point-min) (point-max)))))))

(defun apipy-remote-buf-live-p (buf)
  (process-live-p (apipy-remote-buffer-proc buf)))

(defun apipy-buffer-in-sync ()
  (when apipy--remote-buffer
    (with-slots
          (proc ref)
        (apipy--ensure-backing-buffer)
      (string= (buffer-hash) (apipy-send-sync proc "get_hash_state"
                                              :target ref)))))
(defun apipy-buffer-string ()
  (when apipy--remote-buffer
    (with-slots
          (proc ref)
        (apipy--ensure-backing-buffer)
      (apipy-send-sync proc "get_buffer_contents"
                        :target ref))))

(defun apipy--ensure-backing-buffer ()
  (if (apipy-remote-buf-live-p apipy--remote-buffer)
      apipy--remote-buffer
    (progn
      (setq-local apipy--remote-buffer
                  (apipy-new-buffer (apipy-ensure-kernel))))))

(defmacro with-apipy-buf (proc ref &rest body)
  (let ((sym (gensym)))
    `(if apipy--remote-buffer
         (let* ((,sym (apipy--ensure-backing-buffer))
                (,proc (apipy-remote-buffer-proc
                        ,sym))
                (,ref (apipy-remote-buffer-ref
                       ,sym)))
           ,@body)
       (error "Buffer is not backed")))
  )


(defun apipy-sanity-check-buffer (&optional loud)
  (when apipy--remote-buffer
    (let ((buf (current-buffer))
          (rb apipy--remote-buffer))
      (with-slots
            (proc ref)
          (apipy--ensure-backing-buffer)
        (apipy-send-async proc
                          (lambda (result)
                            (if (string=
                                 result
                                 (buffer-hash buf))
                                (when loud
                                  (message "Already in sync"))
                              (run-with-idle-timer 0.1 nil
                                                   #'apipy-remote-buffer-sync
                                                   rb buf)))
                          "get_hash_state"
                          :target ref)))))


(defun apipy--schedule-sanity-check ()
  (when (or (null apipy--backed-buffer-sanity-timer)
            (timer--triggered apipy--backed-buffer-sanity-timer))
    (let ((buf (current-buffer)))
      (setq-local apipy--backed-buffer-sanity-timer
                  (run-with-idle-timer
                   1 nil
                   (lambda ()
                     (with-current-buffer buf
                       (apipy-sanity-check-buffer))))))))

(defun apipy--backed-buffer-on-insert ()
  (setq-local apipy--buffer-dirty t))

(defun apipy--backed-buffer-sync-hook (start end old-len)
  (when apipy--remote-buffer
    (with-slots (proc ref) (apipy--ensure-backing-buffer)
      (let ((region (buffer-substring-no-properties
                     start
                     end))
            (buf (current-buffer))
            (ilist (list (1- start) (1- end) old-len
                   (buffer-substring-no-properties start end))))
        (with-current-buffer 
            (get-buffer-create "*demo-out*")
          (insert (format "%S\n" ilist)))
        (apipy-send-async
         proc
         (lambda (_)
           (with-current-buffer buf
             (setq-local apipy--buffer-dirty nil)))
         "apply_edit"
         :args (list (1- start) (1- end) old-len
                     (buffer-substring-no-properties
                      start end))
         :target ref))))
  (apipy--schedule-sanity-check))

(defun apipy--delete-buffer-hook ()
  (apipy-delete-buffer apipy--remote-buffer))

(defun apipy--enable-syncronize (buffer)
  (with-current-buffer buffer
    (add-hook 'after-change-functions #'apipy--backed-buffer-sync-hook 0 t)
    (add-hook 'post-self-insert-hook #'apipy--backed-buffer-on-insert 0 t)))

(defun apipy--disable-syncronize (buffer)
  (with-current-buffer buffer
    (remove-hook 'post-self-insert-hook #'apipy--backed-buffer-on-insert t)
    (remove-hook 'after-change-functions #'apipy--backed-buffer-sync-hook t)))

(defun apipy-make-buffer-backed (proc &optional buffer)
  (let ((buf (if buffer
                 (get-buffer buffer)
               (current-buffer))))
    (with-current-buffer buf
      (unless (and apipy--remote-buffer
                   (apipy-remote-buf-live-p apipy--remote-buffer))
        (setq-local apipy--remote-buffer
                    (apipy-new-buffer proc)))
      (apipy--enable-syncronize buf)
      (add-hook 'kill-buffer-hook #'apipy--delete-buffer-hook nil t)
      (apipy--schedule-sanity-check))))

(defun apipy-remove-buffer-backing ()
  (remove-hook 'kill-buffer-hook #'apipy--delete-buffer-hook)
  (apipy--disable-syncronize (current-buffer))
  (apipy-delete-buffer apipy--remote-buffer)
  (setq-local apipy--remote-buffer nil))

(defun apipy--complete-formatter (cmpl)
  (let ((str (aref cmpl 0))
        (typ (aref cmpl 1)))
    (propertize
     str 'type typ)))

(defun apipy--complete-format (cmpls)
  (seq-map #'apipy--complete-formatter cmpls))

(defun apipy--complete-annotate (item)
  (when-let ((type (get-text-property 0 'type item)))
    (format " (%s)" type)))

(defun apipy-call-complete ()
  (when apipy--remote-buffer
    (with-slots (proc ref) (apipy--ensure-backing-buffer)
      (apipy-send-thunk proc
                        "complete_at"
                        :target ref
                        :args (list
                               (line-number-at-pos)
                               (current-column)
                               (buffer-hash))
                        :transform #'apipy--complete-format))))

(defun apipy-call-complete-in ()
  (with-apipy-buf proc ref
                  (apipy-send-thunk proc
                                    "complete_in"
                                    :target ref
                                    :args (list
                                           (buffer-substring-no-properties
                                            (point-min)
                                            (point-max))
                                           (line-number-at-pos)
                                           (current-column))
                                    :transform #'apipy--complete-format)))

(defun apipy-kernel-run (str)
  (apipy-send-thunk
   (apipy-ensure-kernel)
   "eval/run"
   :args (list str)))

(defun apipy-bounds-of-compl ()
  (save-excursion
    (list (progn
            (skip-syntax-backward "w_")
            (point))
          (progn
            (skip-syntax-forward "w_")
            (point)))))

(defun apipy-completion-at-point ()
  (unless (or (syntax-ppss-context (syntax-ppss))
              apipy--buffer-dirty)
    (let ((cmpl (apipy-call-complete-in))
          (bounds (apipy-bounds-of-compl)))
      (append
       bounds
       (list
        (completion-table-with-cache
         (lambda (&rest _)
           (thunk-force cmpl)))
        :annotation-function #'apipy--complete-annotate)))))

(define-minor-mode apipy-backed-buffer-mode
    "Makes the buffer an apipy buffer"
  :lighter nil

  (if apipy-backed-buffer-mode
      (progn
        (apipy-make-buffer-backed
         (or *apipy-kernel*
             (setq *apipy-kernel*
                   (start-apipy-process))))
        (add-hook 'completion-at-point-functions #'apipy-completion-at-point nil t))
    (progn
      (apipy-remove-buffer-backing)
      (remove-hook 'completion-at-point-functions #'apipy-completion-at-point)))
  )

(provide 'apipy)
