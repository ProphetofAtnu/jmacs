;; -*- lexical-binding: t; -*-

(require 'iter-tools)

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

(provide 'core/proc)
