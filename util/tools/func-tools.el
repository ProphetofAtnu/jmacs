;; -*- lexical-binding: t; -*-

(defun juxt (&rest funcs)
  (lambda (arg)
    (mapcar (lambda (func)
              (funcall func arg))
            funcs)))

(defmacro updatef (place func)
  (gv-letplace (get set) place
    (funcall set
             `(funcall ,func ,get))))

(defun update-car (func lst)
  (cons (funcall func (car lst)) (cdr lst)))

(defun map-nth-incl (nth func lst)
  "Call func on every `nth' item in `lst', starting with the car."
  (cl-loop for item in lst
        for cnt from 0
        if (zerop (mod cnt nth))
        collect (funcall func item)
        else
        collect item))

(defun rem-nth-incl (nth lst)
  "Remove every `nth' item in `lst', starting with the car."
  (cl-loop for item in lst
        for cnt from 0
        unless (zerop (mod cnt nth))
        collect item))

(defun rem-indexes (pred lst)
  "Remove every `nth' item in `lst', starting with the nth."
  (cl-loop for item in lst
        for cnt from 0
        unless (funcall pred cnt)
        collect item))

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

(provide 'func-tools)
