;; -*- lexical-binding: t; -*-
(defvar pyel--data-directory (if load-file-name
                                  ;; File is being loaded.
                                  (file-name-directory
                                   load-file-name)
                                ;; File is being evaluated using, for example, `eval-buffer'.
                                default-directory))

(add-to-list 'load-path pyel--data-directory)

(require 'thunk)
(require 'pyel-core)

(defvar-local pyel--unread-lines 0)
(defvar-local pyel--delay-timer nil)

(defvar pyel-ctr 0)
(defvar pyel--handlers (make-hash-table))


(defvar *pyel-kernel* nil)

(defvar *pyel-isolate-context* nil)

(defun pyel-ensure-kernel ()
  (or *pyel-kernel*
      (setq *pyel-kernel*
            (start-pyel-process))))

(defun pyel-reset-kernel-state ()
  (kill-process *pyel-kernel*)
  (setq *pyel-kernel* nil))

(defun pyel--trigger-flush-delayed (proc)
  (with-current-buffer (process-buffer proc)
    (unless (and
             pyel--delay-timer
             (timer--activate pyel--delay-timer))
      (setq-local pyel--delay-timer
                  (run-with-timer
                   0 nil
                   (lambda ()
                     (pyel--flush-messages proc)))))))

(defun pyel--process-filter (proc msg)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (let ((lines (cl-count ?\n msg)))
      (setq-local pyel--unread-lines
                  (+ pyel--unread-lines lines))
      (insert msg)
      (when (> pyel--unread-lines 0)
        (pyel--trigger-flush-delayed proc))))
  )

(defun start-pyel-process ()
  (make-process
   :name "pyel"
   :buffer (generate-new-buffer " *pyel-proc*")
   :command (list "python3" (expand-file-name "run.py" pyel--data-directory))
   :filter #'pyel--process-filter))

(defun pyel-encapsulate (msg)
  (let ((idx (cl-incf pyel-ctr)))
    (cons idx (concat (json-encode (cons (cons :id idx) msg)) "\n"))))


(defun pyel--read-message (proc)
  (with-current-buffer (process-buffer proc)
    (when (> pyel--unread-lines 0)
      (goto-char (point-min))
      (let ((pnt (point))
            (data (json-read)))
        (delete-region pnt (point))
        (cl-decf pyel--unread-lines)
        data))))

(defun pyel--add-tracked (id cb)
  (puthash id
           (lambda (result)
             (remhash id pyel--handlers)
             (funcall cb result))
           pyel--handlers))

(defun pyel--is-response (msg)
  (alist-get 'id msg))

(defun pyel--flush-messages (proc)
  (with-current-buffer
      (process-buffer proc)
    (let (msg)
      (while (setf msg (pyel--read-message proc))
       (if-let ((resid (pyel--is-response msg))
                (handler (gethash resid pyel--handlers nil)))
           
           (if (alist-get 'error msg)
               (message "PYAPI Error: %s" (alist-get 'error msg))
             (funcall handler (alist-get 'result msg))))))))

(cl-defun pyel-send-async (proc cb message)
  (let ((msg (pyel-encapsulate message)))
    (pyel--add-tracked (car msg) cb)
    (process-send-string proc (cdr msg))))

(cl-defun pyel-send-sync (proc message)
  (let ((msg (pyel-encapsulate message))
        done
        cbv)
    (pyel--add-tracked (car msg)
                        (lambda (result)
                          (setf cbv result)
                          (setf done t)))
    (process-send-string proc (cdr msg))
    (while (not done)
      (accept-process-output proc 0 100 t))
    cbv))

(cl-defun pyel-send-thunk (proc message)
  (let ((msg (pyel-encapsulate))
        done
        cbv)
    (pyel--add-tracked (car msg)
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

(defun pyel-make-el ()
  (with-current-buffer (get-buffer-create "*pyel-make-el*")
    (erase-buffer)
    (insert
     (pyel-send-sync
      *pyel-kernel*
      '((:do . "make_el"))))
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t
            (push
             (read (current-buffer))
             forms))
        (error nil))
      (erase-buffer)
      (dolist (f forms)
        (cl-prettyprint f)
        (insert "\n")))))

(defun pyel-read-el ()
  (with-current-buffer
      (get-buffer-create
       "*pyel-make-el*")
    (erase-buffer)
    (insert
     (pyel-send-sync
      (pyel-ensure-kernel)
      '((:do . "make_el"))))
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t
            (push
             (read (current-buffer))
             forms))
        (error nil))
      (dolist (f forms)
        (eval f t)))
    (emacs-lisp-mode)))

(cl-defstruct pyel-obj
  "A bound pyel object that corresponds to a remote object in
python"
  proc
  ref)

(cl-defun pyel-async-call (obj cb method &optional args kwargs)
  (pyel-send-async
   (pyel-obj-proc obj) cb
   (apply #'pyel-core-method method (pyel-obj-ref obj) args kwargs)))

(cl-defun pyel-async-attr (obj cb attr &optional set)
  (pyel-send-async
   (pyel-obj-proc obj) cb
   (apply #'pyel-core-attr attr (pyel-obj-ref obj) set)))

(cl-defun pyel-async-dir (obj cb)
  (pyel-send-async
   (pyel-obj-proc obj) cb
   (apply #'pyel-core-dir obj)))

(cl-defun pyel-thunk-call (obj method &optional args kwargs)
  (pyel-send-thunk
   (pyel-obj-proc obj)
   (apply #'pyel-core-method method (pyel-obj-ref obj) args kwargs)))

(cl-defun pyel-thunk-attr (obj attr &optional set)
  (pyel-send-thunk
   (pyel-obj-proc obj)
   (apply #'pyel-core-attr attr (pyel-obj-ref obj) set)))

(cl-defun pyel-thunk-dir (obj)
  (pyel-send-thunk
   (pyel-obj-proc obj)
   (apply #'pyel-core-dir obj)))

(cl-defun pyel-sync-call (obj method &optional args kwargs)
  (pyel-send-sync
   (pyel-obj-proc obj)
   (apply #'pyel-core-method method (pyel-obj-ref obj) args kwargs)))

(cl-defun pyel-sync-attr (obj attr &optional set)
  (pyel-send-sync
   (pyel-obj-proc obj)
   (apply #'pyel-core-attr attr (pyel-obj-ref obj) set)))

(cl-defun pyel-sync-dir (obj)
  (pyel-send-sync
   (pyel-obj-proc obj)
   (apply #'pyel-core-dir obj)))

(provide 'pyel)
