;;; -*- lexical-binding: t; -*-

(require 'generator)
(require 'dash)

;;; Utility macros
(defmacro +> (&rest forms)
  `(lambda (x) (-> x ,@forms)))

;;; Iterator functions
(iter-defun iter-range (&key from to)
  (let ((cur from))
    (while (< cur to)
      (iter-yield cur)
      (cl-incf cur))))

(iter-defun iter-range-to (to)
  (let ((cur 0))
    (while (< cur to)
      (iter-yield cur)
      (cl-incf cur))))

(defun into-list (iter)
  (let ((r '()))
    (cl-block nil
      (while t
        (condition-case x
            (push (iter-next iter) r)
          (iter-end-of-sequence
           (cl-return (nreverse r))))))))

;;;  Time functions
(defun format-decoded-time (string time)
  (format-time-string string (encode-time time)))

(defun current-decoded-time ()
  (decode-time (current-time)))

(defun decoded-time-plist (time)
  (cl-destructuring-bind (second
                          minute
                          hour
                          day
                          month
                          year
                          dow
                          dst
                          utcoff)
      time
    (list :second second
          :minute minute
          :hour hour
          :day day
          :month month
          :year year
          :dow dow
          :dst dst
          :zone utcoff)))

(defun decoded-time-set (ts &rest timeargs)
  (let ((repl (apply #'make-decoded-time timeargs)))
    (mapcar #'(lambda (x) (or (car x) (cdr x)))
            (-zip repl ts))))

(defun revert-time-plist (plist)
 (mapcar #'cadr
         (-partition 2 plist))) 

;;; Configuration helpers
(defun use-local-pairs (hook pairs)
  (add-hook hook
            (lambda ()
              (require 'elec-pair)
              (make-local-variable
               'electric-pair-pairs)
              (dolist (p pairs)
                (add-to-list 'electric-pair-pairs p))
              (electric-pair-local-mode +1))))


;;; Interactive
(defun get-killable-buffers ()
  (-filter (lambda (b) 
             (not (or (get-buffer-process b)
                      (get-buffer-window b t)
                      (and (buffer-file-name b)
                           (buffer-modified-p b)))))
           (buffer-list)))

(defvar killable-buffers-function #'get-killable-buffers)

(defun kill-extra-buffers ()
  "Kill buffers defined as 'safe' by calling 'killable-buffers-function'"
  (interactive)
  (mapc
   #'kill-buffer
   (funcall
    killable-buffers-function)))

(provide 'core/utility)
