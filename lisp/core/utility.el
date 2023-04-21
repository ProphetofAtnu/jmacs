;;; -*- lexical-binding: t; -*-

(require 'generator)
(require 'dash)
(require 'general)
(require 'hydra)
(require 'time-date)

;;; General Requirements from the "utils" folder
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
            (-zip-pair repl ts))))

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

(defmacro create-open-functions (base args interactive-form &rest body)
  (let* ((base-name (symbol-name base))
         (create-fun (intern (format "%s-create-buffer" base-name))))
    `(prog1 (defun ,create-fun ,args
              ,@body)
            (defun ,base ,args ,interactive-form
                   (switch-to-buffer
                    (funcall (quote ,create-fun) ,@args)))
            (defun ,(intern (format "%s-other-window" base-name))
                ,args
              ,interactive-form
              (switch-to-buffer-other-window
               (funcall (quote ,create-fun) ,@args)))
            (defun ,(intern (format "%s-other-window-noselect" base-name))
                ,args
              ,interactive-form
              (save-selected-window
                (switch-to-buffer-other-window
                 (funcall (quote ,create-fun) ,@args))))
            )))


(defun add-temp-hook (hook fun)
  (let ((sym (gensym "temp-hook-")))
    (fset sym
          (lambda (&rest args)
            (apply fun args)
            (remove-hook hook sym)))
    (add-hook hook sym)))

(defun get-current-bindings ()
  (cl-loop for c being the key-seqs of (current-local-map)
        using (key-bindings b)
        collect (cons (mapcar 'single-key-description c) b)))

;;; Markers
(defmacro with-marker-location (marker &rest body)
  `(with-current-buffer (marker-buffer ,marker)
     (save-excursion
       (goto-char (marker-position ,marker))
       ,@body)))

(defmacro with-moving-marker (marker &rest body)
  `(with-current-buffer (marker-buffer ,marker)
     (save-excursion
       (goto-char (marker-position ,marker))
       (let  ((res (progn ,@body)))
         (move-marker ,marker (point))
         res))))


(defun marker-eobp (mark)
  (with-current-buffer (marker-buffer mark)
    (= (marker-position mark) (point-max))))

(defun format-data-human-readable (bytes-count format &optional metric?)
  (let ((factor (if metric?
                    1000.0
                  1024.0)))
    (cl-case format
      (KiB (/ bytes-count factor))
      (KB (/ bytes-count factor))
      (kb (/ bytes-count factor))
      (MiB (/ bytes-count (expt factor 2)))
      (MB (/ bytes-count (expt factor 2)))
      (mb (/ bytes-count (expt factor 2)))
      (GiB (/ bytes-count (expt factor 3)))
      (GB (/ bytes-count (expt factor 3)))
      (gb (/ bytes-count (expt factor 3))))))

(defun bytes-parse-string (fmt &optional metric?)
  (save-match-data
    (unless (string-match "\\([[:digit:]]+\\)\\([KkMmGg]\\)[iI]?[bB]" fmt)
      (error "byte string unparseable %s" fmt))
    (let ((num (string-to-number
                (match-string 1 fmt)))
          (spec
           (string-to-char (downcase (match-string 2 fmt))))
          (factor (if metric?
                      1000
                    1024)))
      (cl-case spec
        (?k (* num factor))
        (?b (* num (expt factor 2)))
        (?g (* num (expt factor 3)))))))


(defmacro js/to-repeatable (fun &optional force)
  (let ((fn-name (intern (format "$rep/%S" fun))))
    (if (and (not force) (functionp fn-name))
        `(function ,fn-name)
      `(defun ,fn-name (&rest args)
         (interactive)
         (apply (function ,fun) args)
         (set-transient-map
          (let ((km (make-sparse-keymap)))
            (keymap-set km (single-key-description last-input-event) (function ,fun))
            (set-transient-map km t)))))))

(provide 'core/utility)
