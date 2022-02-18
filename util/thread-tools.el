;;; -*- lexical-binding: t; -*-
(require 'cl-macs)

(cl-defstruct tfuture
  "Threaded future for use with the builtin emacs threading system"
  (sync (make-condition-variable (make-mutex)))
  (done nil)
  result)

(defsubst new-cond-var ()
  (make-condition-variable (make-mutex)))

(cl-defgeneric get-mutex (holder)
  "Return the mutex object contained within the argument `holder'"
  (cond
    ((mutexp holder) holder)
    ((condition-variable-p holder) (condition-mutex holder))))

(cl-defgeneric get-cond (holder)
  "Return the cond object contained within the argument `holder'.
Acts as a passthrough for actual condition variables"
  (cond
    ((condition-variable-p holder) holder)
    (t nil)))

(cl-defgeneric future-complete (future value)
  "Complete the future with `value'")

(cl-defgeneric future-result (future)
  "Return the result of the generic future")

(cl-defmethod get-mutex ((holder tfuture))
  (condition-mutex (tfuture-sync holder)))

(cl-defmethod get-cond ((holder tfuture))
  (tfuture-sync holder))

(cl-defmethod future-complete ((future tfuture) value)
  (with-lock future
    (setf (tfuture-result future) value
          (tfuture-done future) t)
    (condition-notify (tfuture-sync future))))

(cl-defmethod future-result ((future tfuture))
  (tfuture-result future))

(defmacro with-lock (locker &rest body)
  "Wrapper for `with-mutex' that uses a generic variable that
references a mutex"
  `(with-mutex (get-mutex ,locker)
     ,@body))

(defun await (future)
  "Await a condition variable contained within the argument.
Condition is aquired using `get-cond'. If there is no cond
associated with the item, it is returned as is"
  (with-lock future
    (condition-wait (get-cond future))))

(defun run-async (func)
  (let ((future (make-tfuture)))
    (make-thread
     #'(lambda ()
         (future-complete future (funcall func))))
    future))
