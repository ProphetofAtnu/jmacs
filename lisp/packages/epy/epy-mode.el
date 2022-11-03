;; -*- lexical-binding: t; -*-
(require 'epy)

(defgroup epy-mode nil
  "An interactive interface for epy")

(defvar epy-minor-mode nil)

(defvar-local epy-mode--buffer-source nil)
(defvar epy-using-sessions nil)
(defvar epy--mode-last-open-process nil)
(defvar epy-mode-flush-type #'epy-run-flush-direct-on-new-messages
  "The default flush type for epy-mode buffers.")

(defun epy-mode--start-or-reuse ()
  (setq epy-mode--buffer-source
        (if (and epy--mode-last-open-process
                 (epy-live-p epy--mode-last-open-process))
            epy--mode-last-open-process
          (setf epy--mode-last-open-process
                (epy-process :on-message
                             epy-mode-flush-type))))
  (cl-pushnew #'epy-mode-message-on-close
        (oref epy-mode--buffer-source on-closed))
  epy-mode--buffer-source)

(defun epy-capf-annotation-func (item)
  (when-let ((p (get-text-property 0 'py-kind item)))
    (format "   %s" p)))

(defun epy-completion-table-with-cb ()
  (let* ((cline (1+ (current-line)))
         (ccol (current-column))
         (buff (buffer-substring-no-properties (point-min) (point-max)))
         (data nil)
         (waiting t)
         (done-cb
           (lambda (result)
             (setf
              data (cl-loop for (cstr desc) in result
                         collect (propertize cstr 'py-kind desc))
              )
             (setq waiting nil))))
    (run-with-timer 0 nil #'epy-send
                    epy-mode--buffer-source (list
                                     :endpoint "code"
                                     :method "capf"
                                     :args (list buff cline ccol))
                    done-cb)
    (lambda (str)
      (while-no-input
        (when waiting
          (accept-process-output (oref epy-mode--buffer-source proc) 0 500 t)))
      data)))



(defun epy-capf-local-bounds ()
    (save-excursion
     (let ((beg (progn
                  (skip-syntax-backward "w_")
                  (point)))
           (end (progn (skip-syntax-forward "w_")
                       (point))))
       (cons beg end))))

(defun epy-completion-at-point ()
  (when epy-mode--buffer-source
    (-let (((beg . end) (epy-capf-local-bounds)) 
           (ct (completion-table-dynamic (epy-completion-table-with-cb))))
      (list beg end ct
            :annotation-function #'epy-capf-annotation-func))))


(defun epy-mode--print-results (result)
  (cl-destructuring-bind (rlist stdo) result
    (message "%s\nSTDOUT: %s" (string-join rlist "\n") stdo)))

(defun epy-mode-run (code)
  (run-with-timer 0 nil #'epy-send
                  epy-mode--buffer-source (list
                                           :endpoint "code"
                                           :method "run"
                                           :args (list code))
                  #'epy-mode--print-results))


(defun epy-create-scratch ()
  (interactive)
  (let ((buf (generate-new-buffer "*epy-scratch*")))
    (with-current-buffer buf
      (python-mode)
      (epy-minor-mode +1))
    (pop-to-buffer buf)))

(defun epy-mode-run-buffer ()
  (interactive)
  (unless epy-minor-mode
    (error "Not in an epy enabled buffer"))
  (epy-mode-run
   (buffer-substring-no-properties (point-min) (point-max))))

(defun epy-mode-run-line ()
  (interactive)
  (unless epy-minor-mode
    (error "Not in an epy enabled buffer"))
  (epy-mode-run
   (buffer-substring-no-properties (point-at-bol)
                                   (point-at-eol))))

(defun epy-mode-run-region (beg end)
  (interactive "r")
  (unless epy-minor-mode
    (error "Not in an epy enabled buffer"))
  (epy-mode-run
   (buffer-substring-no-properties beg
                                   end)))

(defun epy-mode-restart ()
  (interactive)
  (when epy-mode--buffer-source
    (with-slots (proc) epy-mode--buffer-source
      (kill-process proc)))
  (setf epy-mode--buffer-source
        (setf epy--last-open-process
              (epy-process :on-message
                           epy-mode-flush-type)))
  (push #'epy-mode-message-on-close
        (oref epy-mode--buffer-source on-closed)))

(defun epy-mode-message-on-close ()
  (message "Epy process terminated"))

(define-minor-mode epy-minor-mode
    "A minor mode for interactive python development and execution."
  :lighter nil
  :group 'epy-mode
  (if epy-minor-mode
      (progn
        (epy-mode--start-or-reuse)
        (add-hook 'completion-at-point-functions #'epy-completion-at-point nil t))
    (progn
      (when epy-mode--buffer-source
        (with-slots (proc) epy-mode--buffer-source
          (kill-process proc))))))


(provide 'epy-mode)
