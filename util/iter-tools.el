;; -*- lexical-binding: t; -*-

(require 'generator)

(eval-when-compile
  (defmacro iter-tool--with-moving-marker
      (marker &rest body)
    `(with-current-buffer
         (marker-buffer ,marker)
       (save-excursion
         (goto-char
          (marker-position ,marker))
         (let
             ((res
               (progn ,@body)))
           (move-marker ,marker
                        (point))
           res)))))

(defun iter-tool--marker-eobp
    (mark)
  (with-current-buffer
      (marker-buffer mark)
    (=
     (marker-position mark)
     (point-max))))

;;; Iterator functions
(cl-iter-defun iter-range (&key to (from nil) (by nil) (incl nil))
  "Create an iterator from a range"
  (let ((cur (or from 0))
        (cmp (if incl
                 #'<=
               #'<)))
    (while
        (funcall cmp cur to)
      (iter-yield cur)
      (if (functionp by)
          (setf cur (funcall by cur))
        (cl-incf cur)))))

(iter-defun iter-range-to (to)
  "Create an iterator from a range"
  (let ((cur 0))
    (while (< cur to)
      (iter-yield cur)
      (cl-incf cur))))

(iter-defun iter-vector (arr)
  "Create an iterator from a vector"
  (let ((idx -1))
    (while (< (cl-incf idx) (length arr))
      (iter-yield (aref arr idx)))))

(iter-defun iter-list (l)
  "Create an iterator from a list"
  (let ((lst l)
        pos)
    (while
        (setf pos (car-safe lst))
      (setf lst (cdr lst))
      (iter-yield pos))))

(iter-defun iter-hash-table (ht)
  "Create an iterator from a hash table"
  (let ((keys (hash-table-keys ht))
        k)
    (while
        (setf k (pop keys))
      (iter-yield (cons k (gethash k ht))))))

(iter-defun iter-signaling (fun)
  "Create an iterator out of a callable function that signals when
it can't run any more."
  (let (v)
    (condition-case nil
        (while t
          (iter-yield (funcall fun)))
        ('error nil))))

(iter-defun iter-buffer-lines (buffer)
  (let ((mark (with-current-buffer buffer
                (point-min-marker))))
    (while (not (iter-tool--marker-eobp mark))
      (iter-yield
       (iter-tool--with-moving-marker
           mark
         (forward-line 1)
         (buffer-substring-no-properties
          mark
          (point)))))))

(iter-defun split-string-lazily (str sep)
  (let ((pos 0)
        (tpos 0)
        (len (length str)))
    (while (< (setf tpos
                    (or (string-match-p sep str pos)
                        len))
              len)
      (iter-yield (substring str pos (1+ tpos)))
      (setf pos (1+ tpos)))
    (unless (= tpos pos)
      (substring str pos tpos))))

(defun iter-transform (func itr)
  (lambda (op val)
    (funcall func (funcall itr op val))))

(defun iterator-to-list (iter)
  (let (items)
    (iter-do (i iter)
      (push i items))
    (nreverse items)))

(cl-defgeneric iterate-over (thing))

(cl-defmethod iterate-over ((thing cons))
  (iter-list thing))

(cl-defmethod iterate-over ((thing vector))
  (iter-vector thing))

(cl-defmethod iterate-over ((thing hash-table))
  (iter-hash-table thing))

(cl-defmethod iterate-over ((thing string))
  (iter-vector thing))

(defun make-line-iterator-process-filter (&optional on-line)
  (let ((frag nil)
        (handler (or
                  on-line
                  (lambda (proc hline)
                    (with-current-buffer (process-buffer proc)
                      (save-excursion
                        (goto-char (point-max))
                        (insert hline)))))))
   (lambda (proc str)
     (let (done
           (itr (split-string-lazily
                 (concat frag str)
                 "[\n\r]")))
       (while (let ((next (condition-case x
                              (iter-next itr)
                            (iter-end-of-sequence
                             (progn
                               (setf done t)
                               (cons :frag (cdr x)))))))
                (pcase next
                  (`(:frag . ,val)
                    (setf frag val))
                  ((pred stringp)
                   (funcall handler proc next)))
                (not done)))))))

(provide 'iter-tools)
