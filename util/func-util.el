;; -*- lexical-binding: t; -*-

(defun juxt (&rest funcs)
  (lambda (arg)
    (mapcar (lambda (func)
              (funcall func arg))
            funcs)))

(defun compose (&rest funcs)
  (lambda (arg)
    (let ((co arg))
      (dolist (f funcs)
        (setf co (funcall f co)))
      co)))

(defmacro lambda-first (&rest body)
  `(lambda (arg)
    (thread-first arg
                  ,@body)))

(defmacro lambda-last (&rest body)
  `(lambda (arg)
     (thread-last arg
                   ,@body)))

(provide 'func-util)
