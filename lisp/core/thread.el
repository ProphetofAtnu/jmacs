;;; -*- lexical-binding: t; -*-

(defun dispatch-new-thread (f)
    (make-thread f))

(defvar *default-dispatcher* #'dispatch-new-thread)

(defun await-cond (c)
  (with-mutex (condition-mutex c)
    (condition-wait c)))

(defmacro when-notified (c &rest body)
  `(with-mutex (condition-mutex c)
     (condition-wait c)
     ,@body))

(cl-defgeneric get-lock (locker)
  (when (condition-variable-p locker)
      (condition-mutex locker)))

(cl-defgeneric await (awaitable)
  (if (condition-variable-p awaitable)
      (await-cond awaitable))
  awaitable)

(cl-defgeneric deliver (deliv value))


(defmacro with-lock (lockable &rest body)
  `(with-mutex (get-lock lockable)
     ,@body))  

;; future structures
(cl-defstruct future
  (sync (make-condition-variable (make-mutex)))
  result)

(cl-defmethod get-lock ((locker future))
  (condition-mutex (future-sync locker)))

(cl-defmethod deliver ((deliv future) value)
  (unless (not (null (future-result deliv)))
    (let ((c (future-sync deliv)))
      (with-mutex (condition-mutex c)
        (setf (future-result deliv) value)
        (condition-notify c t)))))

(cl-defmethod await ((awaitable future))
  (if-let ((v (future-result awaitable)))
      v
    (with-mutex
        (get-lock awaitable)
      (condition-wait (future-sync awaitable))
      (future-result awaitable))))

(defun async (func)
  (let ((fut (make-future)))
    (funcall
     *default-dispatcher*
     #'(lambda ()
         (deliver fut (funcall func))))
    fut))

(cl-defstruct stream
  (mx (make-mutex) :read-only t)
  (state :open))

(provide 'core/thread)
