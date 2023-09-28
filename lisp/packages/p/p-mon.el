;; -*- lexical-binding: t; -*-

(require 'oclosure)

(oclosure-define p-once-func
  "A function that only runs once, returning after it's first
invocation."
  (has-run :mutable t :type bool)
  )

(defmacro p-lambda-once (args &rest body)
  (declare (indent 2))
  `(oclosure-lambda (p-once-func (has-run nil)) ,args
     (unless has-run
       (setf has-run t)
       ,@body)))

(defun p-exit-hook-sentinal (func)
  "Return a sentinal lambda that invokes `func' with the process and
final message when `process-live-p' returns false."
  (let ((has-run nil))
    (lambda (proc message)
      (if (process-live-p proc)
	  nil
	(unless has-run
	  (funcall func proc message)
	  (setf has-run t))))))

(provide 'p-mon)
