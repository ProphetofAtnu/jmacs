;; -*- lexical-binding: t; -*-


(defvar epy-dir-name (expand-file-name "lisp/packages/epy" user-emacs-directory))
(defvar epy-idle-flush-delay-time .1)


(defun epy-create-process (&optional name buffer)
  (let ((default-directory epy-dir-name))
    (make-process
     :name (or name "*epy*")
     :buffer (or buffer (generate-new-buffer "*epy*"))
     :command '("python3" "-m" "epypy"))))

(defclass epy-process ()
  ((name :initarg :name :initform "epy-proc")
   (proc :initform nil)
   (buffer :initform nil)
   (unread :initform 0 :accessor epy-unread)
   (ctr :initform 0)
   (on-message :initform nil :initarg :on-message)
   (cb-table :initform (make-hash-table))
   (on-closed :initform nil)))

(defun epy--process-filter (ep)
  (lambda (proc str)
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (with-slots (on-message unread) ep
       (seq-do (lambda (c)
                 (insert-char c)
                 (if (eq c ?\n)
                     (cl-incf unread)))
               str)
       (when (and (> unread 0) on-message)
         (funcall on-message ep))))))

(defun epy--process-sentinel (ep)
  (lambda (proc str)
    (when (string-prefix-p
           "finished"
           str)
      (run-hooks (oref ep on-closed)))))

(cl-defmethod initialize-instance :after ((class epy-process) &rest slots)
  (with-slots (name buffer proc) class
    (setf buffer (or buffer (generate-new-buffer " *epy-process*")))
    (let ((default-directory epy-dir-name))
      (setf proc
       (make-process
        :name name
        :buffer buffer
        :sentinel (epy--process-sentinel class)
        :filter (epy--process-filter class)
        :command '("python3" "-m" "epypy"))))))

(cl-defmethod epy-gen-id ((process epy-process))
  (with-slots (ctr) process
    (cl-incf ctr)))

(cl-defgeneric epy-format (thing)
  "Format an object to be sent to the epy-process. This method
should make something into a plist."
  thing)

(cl-defmethod epy-flush (epyp)
  (with-slots (unread buffer cb-table on-message) epyp
    (with-current-buffer buffer
      (goto-char (point-min))
      (condition-case nil
          (while (> unread 0)
            (goto-char (point-min))
            (let* ((json-object-type 'plist)
                   (json-array-type 'list)
                   (val (json-read))
                   (id (plist-get val :id))
                   (result (plist-get val :result))
                   (err (plist-get val :error))
                   (cb (gethash id cb-table)))
              (if cb
                (unwind-protect
                     (if err
                         (funcall cb err)
                       (funcall cb result))
                  (remhash id cb-table))))
            (delete-line))
        ('error (progn
                  (setf unread 0)
                  (erase-buffer))))
      )))

(cl-defmethod epy-send ((process epy-process) msg cb)
  (with-slots (proc cb-table) process
    (let ((id (epy-gen-id process)))
      (when cb
        (setf (gethash id cb-table) cb))
      (process-send-string
       proc
       (concat
        (json-encode
         (plist-put
          (epy-format msg)
          :id id))
        "\n")))))

(defun epy-run-flush-on-new-messages (ep-proc)
  "For use in the `on-message' handler of epy-process. When a
message is recieved, it is queued to be handled in the next loop
with a timer."
  (run-with-timer 0 nil #'epy-flush ep-proc))

(defun epy-run-idle-flush-on-new-messages (ep-proc)
  "For use in the `on-message' handler of epy-process. When a
message is recieved, an idle timer is queued as determined by the
variable `epy-idle-flush-delay-time'."
  (run-with-idle-timer epy-idle-flush-delay-time nil #'epy-flush ep-proc))

(defun epy-run-flush-direct-on-new-messages (ep-proc)
  "For use in the `on-message' handler of epy-process. When a
message is recieved, epy-flush is run directly in the lexical context
of the process filter."
  (epy-flush ep-proc))


(provide 'epy)
