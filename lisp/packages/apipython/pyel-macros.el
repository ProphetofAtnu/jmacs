;; -*- lexical-binding: t; -*-

'(pyel-let ((os (import strings)))
  (print
   (call os :listdir)))

(let ((proc (pyel-ensure-kernel)))
  (pyel-send-async
   proc
   (lambda (res)
     (let ((os (make-pyel-obj
                :proc proc
                :ref res)))
       (pyel-async-call
        os
        (lambda (cr)
          (print cr))
        :listdir)))
   (pyel-core-import-module "os")))


(defmacro pyel-let-import (module &rest body)
  (let ((sym (gensym)))
    `(pyel-send-async
      (lambda (,sym)
        (let ((,module
               (make-pyel-obj
                :proc proc
                :ref ,sym)))
          ,@body))
      (pyel-core-import-module ,module))))

(cl-defmacro pyel-let-call (binding (obj method &optional args kwargs) &rest body)
  `(pyel-async-call
    ,obj
    (lambda (,binding)
      ,@body)
    ,method ,args ,kwargs))

(pyel-let-call x (obj :test)
               (print x))


(defun pyel-macro--trigger (count cb)
  (let ((c count))
    (lambda (res)
      (cl-decf count)
      (if (<= count 0)
          (funcall cb)))))

(defmacro pyel-let (vars &rest body)
  (let ((bind (mapcar #'car vars))
        (expr (mapcar #'cdar vars)))

    ))

(provide 'pyel-macros)
