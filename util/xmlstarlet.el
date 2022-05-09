;; -*- lexical-binding: t; -*-

;;; Example 
;; (xmlstarlet-sel-to-string
;;  "clog.xml"
;;  :m "//ItemConfig"
;;  :var "lc=Property[last()]"
;;  :m "Property"
;;  :v "@name"
;;  :o ":"
;;  :v "."
;;  :i ".!=$lc"
;;  :o "|"
;;  :b :b :n)

;;; xmlstarlet wrappers
(defun xmlstarlet--into-flags (args)
  (let (arg-fmt
        (arg-tmp nil))
    (dolist (a args (setf
                     arg-fmt
                     (flatten-list
                      (nreverse
                       (if arg-tmp
                           (cons (nreverse arg-tmp)
                                 arg-fmt)
                         arg-fmt)))))
      (if (keywordp a)
          (progn
            (when arg-tmp
              (push (nreverse arg-tmp) arg-fmt))
            (setf arg-tmp (list
                           (let ((argn (string-trim-left (symbol-name a) ":")))
                             (if (length< argn 2)
                                 (concat "-" argn)
                               (concat "--" argn))))))
        (push (cond 
                ((stringp a) (format "'%s'" a))
                ((symbolp a) (symbol-name a))
                (t (format "%S" a)))
              arg-tmp)))
    arg-fmt)
  )

(defun xmlstarlet-sel (buffer &rest args)
  "Send `buffer' to 'xmlstarlet sel' as a template. Single
character flags are represented as keyword arguments. The
returned value is the output buffer. "
  (let ((out-buffer (generate-new-buffer
                     " *xmlstarlet-sel*"))
        (cmd (concat "xmlstarlet sel -t "
                     (string-join
                      (xmlstarlet--into-flags args)
                      " "))))
    (with-current-buffer
        (get-buffer buffer)
      (call-shell-region
       (point-min)
       (point-max)
       cmd nil out-buffer))
    out-buffer))

(defun xmlstarlet-sel-display (buffer &rest args)
  "Send `buffer' to 'xmlstarlet sel' as a template. Single
character flags are represented as keyword arguments. The
returned value is the output buffer. "
  (let ((buf (apply 'xmlstarlet-sel buffer args)))
    (with-current-buffer buf
      (add-hook 'quit-window-hook 
                (lambda () (kill-buffer buf)) 0 t)
      (special-mode))
    (pop-to-buffer buf)))

(defun xmlstarlet-sel-to-string (buffer &rest args)
  "Send `buffer' to 'xmlstarlet sel' as a template and return the
results as a string. Single character flags are represented as
keyword arguments."
  (let ((buf (apply #'xmlstarlet-sel buffer args))
        str)
    (with-current-buffer buf
      (setf str (buffer-substring-no-properties (point-min)
                                                (point-max))))
    (kill-buffer buf)
    str))

(defmacro xmlstarlet-sel-with-buffer (buffer-and-args &rest body)
  "Send `buffer-and-args' to `xmlstarlet-sel' and run `body' in the
context of the result buffer. Returns the result of `body' and
deletes the buffer on exit."
  (let
      ((buf (make-symbol "*xmlstarlet-buf*"))
       (result (make-symbol "*xmlstarlet-result*")))
    `(let ((,buf (xmlstarlet-sel ,@buffer-and-args))
             ,result)
         (unwind-protect
              (setf ,result
                    (with-current-buffer ,buf
                      (goto-char (point-min))
                      ,@body
                      ))
           (kill-buffer ,buf))
         ,result)))

(defun xmlstarlet-cleanup-buffers ()
  (interactive)
  (cl-loop for x being the buffers
        when (string-prefix-p " *xmlstarlet" (buffer-name x))
        do (kill-buffer x)))

(provide 'xmlstarlet)
