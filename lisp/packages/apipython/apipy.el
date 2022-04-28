;; -*- lexical-binding: t; -*-

(require 'thunk)

(defvar-local apipy--unread-lines 0)
(defvar-local apipy--delay-timer nil)

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

(provide 'apipy)
