;; -*- lexical-binding: t; -*-

(defun find-atom-root-children (window)
  (let (children
        (windows (window-list-1)))
    (dolist (win windows)
      (when (eq window (window-atom-root win))
        (push win children)))
    children))

(defun delete-atomic-windows (&optional window)
  (let* ((win (or window (selected-window)))
         (root (or (window-atom-root win)
                   (error "window is not atomic %S" win)))
         (children (find-atom-root-children root))
         (main (cl-find-if
                (lambda (w)
                  (eq 'main (window-parameter w 'window-atom)))
                children)))
    (select-window main)
    (mapc (lambda (w)
            (set-window-parameter w 'window-atom nil)
            (unless (eq main w)
              (delete-window w)))
          children)))

(defun display-as-associated-window (buffer alist)
  (let ((assoc (or (alist-get
                    'associated-window alist nil)
                   (selected-window)))
        (win (display-buffer-in-atom-window
              (get-buffer buffer)
              alist)))
    (set-window-parameter win 'associated-window assoc)))

(defun assoc-window-buffer (&optional window)
  (window-buffer
   (window-parameter
    (or window (selected-window))
    'associated-window)))

(defmacro with-assoc-window-buffer (window-or-nil &rest body)
  (let ((asc (gensym "assoc-win")))
    `(when-let
         ((,asc (assoc-window-buffer ,window-or-nil)))
       (with-current-buffer ,asc
         ,@body))))

(defun assoc-window-buffer-name (&optional window)
  (with-assoc-window-buffer window
    (buffer-name)))

(defun kill-associated-windows (&optional window)
  (let ((assocs
         (seq-filter
          (lambda (w)
            (eq (window-parameter
                 w
                 'associated-window)
                (or window (selected-window))))
          (window-list-1))))
    (dolist (w assocs)
      (set-window-parameter w 'window-atom nil)
      (delete-window w))))

(provide 'window-association)
