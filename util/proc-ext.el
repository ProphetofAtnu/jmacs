;; -*- lexical-binding: t; -*-

(defconst async-command-buffer-basename " *async-command*")
(defvar async-command--buffer-disposables nil)
(defvar async-command-buffer-disposal-timer nil)
(defconst async-command-callback-key 'async-process-finished-cb)

(defun async-command-dispose-buffers ()
  (while-let ((b (pop async-command--buffer-disposables)))
    (kill-buffer b))
  (setf async-command-buffer-disposal-timer nil))

(defun async-command-queue-deletion (buff)
  (cl-pushnew buff async-command--buffer-disposables)
  (unless async-command-buffer-disposal-timer
    (setf async-command-buffer-disposal-timer
          (run-with-idle-timer 1 nil #'async-command-dispose-buffers))))

(defun async-command--buffer-sentinal (proc msg)
  (unless (process-live-p proc)
    (unwind-protect
	(let ((cb (process-get proc async-command-callback-key)))
	  (when cb
            (funcall cb proc)))
      (async-command-queue-deletion (process-buffer proc)))))

(defun exec-with-callback (callback command &rest args)
  (let* ((abuf (generate-new-buffer async-command-buffer-basename t))
         (prc (make-process
               :connection-type 'pipe
               :buffer abuf
               :command (cons command args)
               :noquery t
               :name (format "async-process %s" command)
               :sentinel #'async-command--buffer-sentinal)))
    (process-put prc async-command-callback-key callback)
    prc))

(defun cancel-process-callback (prc)
  (process-put prc async-command-callback-key nil)
  (kill-process prc))

(provide 'proc-ext)
