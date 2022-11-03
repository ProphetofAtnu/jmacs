;; -*- lexical-binding: t; -*-

(require 'func-tools)
(require 'tabulated-list)

(defvar-local tlist-default-padding 5)

(defun tlist-refresh ()
  (tabulated-list-print)
  (tabulated-list-init-header))

(defun plist-table-data (plists)
  (let* ((keys (seq-uniq 
                (rem-indexes #'cl-oddp (flatten-list plists))))
         (values (mapcar
                  (lambda (l)
                    (mapcar (lambda (i) (plist-get l i)) keys))
                  plists)))
    (cons keys values)))

;; TODO
;; (defun alist-table-data (alists)
;;   (let* ((keys (seq-uniq 
;;                 (mapcar #'car (flatten-list alists))))
;;          (values (mapcar
;;                   (lambda (l)
;;                     (mapcar (lambda (i) (alist-get l i)) keys))
;;                   alists)))
;;     (cons keys values)))

(defun tlist-data-format (data)
  (let ((ctr -1))
    (seq-map
     (lambda (row)
       (list
        (setf ctr (1+ ctr))
        (seq-into
         (seq-map
          (apply-partially #'format "%s")
          row)
         'vector)))
     data)))

(defun tlist-data-wrap (data)
  (lambda ()
    (if (functionp data)
        (tlist-data-format (funcall data))
      (tlist-data-format data))))

(cl-defun tlist-headers-make (seq &key (width 20) (sortable t))
  (seq-into 
   (seq-map (lambda (itm)
              `(,(format "%s" itm) ,width ,sortable))
            seq)
   'vector))


;; (let* ((bufs (buffer-list))
;;        (data (mapcar (juxt #'buffer-name
;;                            #'buffer-file-name
;;                            #'identity)
;;                      bufs))
;;        (columns '("Name" "File" "Repr")))
;;   (with-current-buffer (get-buffer-create "*buftbl*")
;;     (tlist-initialize-simple  columns
;;                               data)))

(cl-defun tlist-initialize (columns data)
  (let ((cols (tlist-headers-make columns))
        (data (tlist-data-format data)))
    (setq-local tabulated-list-format cols)
    (setq-local tabulated-list-entries data)
    (tabulated-list-mode)
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defun tlist-get-data ()
  (if (functionp
       tabulated-list-entries)
      (funcall
       tabulated-list-entries)
    tabulated-list-entries))

(defun tlist--calculate-autofit-columns (&optional padding)
  (let* ((data (if (functionp tabulated-list-entries)
                   (funcall tabulated-list-entries)
                 tabulated-list-entries))
         (entry-len (length (cadar data)))
         (len-tracker (make-vector entry-len 0)))
    (cl-flet
        ((len-update (row)
           (dotimes (i entry-len)
             (setf (aref len-tracker i)
                   (max (aref len-tracker i)
                        (length (aref row i)))))))
      (seq-do (lambda (x)
                (len-update (cadr x)))
              data)
      (if padding
          (dotimes (i entry-len)
            (setf (aref len-tracker i)
                  (+ padding (aref len-tracker i)))))
      len-tracker)))

(defun tlist-get-column-idx (name)
  (seq-position tabulated-list-format name
                (lambda (x y)
                  (equal
                   (car x)
                   y))))

(defun tlist-get-column-index-at-pos (pos)
  (tlist-get-column-idx
   (get-text-property
    pos
    'tabulated-list-column-name)))

(defun tlist-get-column-data (name)
  (let* ((idx (tlist-get-column-idx name))
         (data (tlist-get-data)))
    (mapcar (lambda (row)
              (aref (cadr row) idx))
            data)))

(defun tlist-get-column-max-size (name)
  (let* ((col name)
         (idx (tlist-get-column-idx col))
         (max-length (seq-max (seq-map #'length
                                       (tlist-get-column-data col)))))
    (cons idx max-length)))

(defun tlist-autofit-column (pos &optional padding)
  (interactive "d")
  (cl-destructuring-bind (i . l) (tlist-get-column-max-size
                                  (get-text-property pos 'tabulated-list-column-name))
    (setf (nth 1 (aref tabulated-list-format i))
          (+ l (or padding tlist-default-padding)))
    (tlist-refresh)))

(defun tlist-autofit-columns (&optional padding)
  (interactive)
  (let ((sizes (tlist--calculate-autofit-columns
                (or padding tlist-default-padding))))
    (seq-do-indexed
     (lambda (v i)
       (setf (nth 1 (aref tabulated-list-format i))
             v))
     sizes))
  (tlist-refresh))


(provide 'tlist-tools)
