;; -*- lexical-binding: t; -*-

(require 'iter-tools)

(defun on-dead-sentinel (fn)
  (let ((has-run nil))
    (lambda (proc evt)
      (if (process-live-p proc)
	  nil
	(unless has-run
	  (setf has-run t)
	  (funcall fn proc evt))))))

(defun proc--make-iterator-process-args ()
  (let* ((stack nil)
         (done nil))
    (list
     (lambda (proc str)
       (push str stack))
     (lambda (proc state)
       (unless (string-match-p "\\(run\\|open\\)" state)
         (setf done t)))
     (funcall
      (iter-lambda
          ()
          (while (or stack (not done))
            (print done)
            (iter-yield
             (if stack (pop stack)
               :wait))))))))

(defun call-with-buffer (shell-cmd output &optional buffer)
  (with-current-buffer (or buffer
                           (current-buffer))
    (call-shell-region (point-min)
                       (point-max)
                       shell-cmd
                       nil
                       (with-current-buffer
                           (get-buffer-create output)
                         (erase-buffer)
                         (current-buffer)))))

(defun run-shell-command-on-buffer (command)
  (interactive
   (list (read-shell-command "Pipe to: ")))
  (let ((out (format "*%s-cmd*" (buffer-name)))
        (inhibit-read-only t))
    (call-with-buffer command out)
    (with-current-buffer out
      (special-mode))
    (display-buffer out)))

(defun run-shell-command-on-other-buffer (buffer command)
  (interactive
   (list
    (read-buffer "Buffer: ")
    (read-shell-command "Pipe to: ")))
  (with-current-buffer buffer
    (run-shell-command-on-buffer command)))


(provide 'core/proc)
