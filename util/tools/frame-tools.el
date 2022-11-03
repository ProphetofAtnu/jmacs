;; -*- lexical-binding: t; -*-
;; WIP

(require 'hydra)

(defgroup frame-tools ()
  "Customize group for frame-tools lisp module."
  :group 'extensions
  :prefix "frame-tools-")

(defcustom frame-tools-move-offsets '(0 . 0)
  "The offets that a frame is adjusted after moving."
  :group 'frame-tools
  :type '(cons))


(defcustom frame-tools-move-coarse-distance 100
  "The offets that a frame is adjusted after moving."
  :group 'frame-tools
  :type '(number))

(defcustom frame-tools-move-fine-distance 5
  "The offets that a frame is adjusted after moving."
  :group 'frame-tools
  :type '(number))

(defun recalculate-frame-move-offsets (&optional x y)
  (let* ((frame (make-frame))
         (loc (cons (or x 500)
                    (or y 500)))
         (frame-loc (progn
                      (set-frame-position frame (car loc) (cdr loc))
                      ;; Long enough for any windowing changes to happen
                      (sit-for 1)
                      (frame-position frame))))
    (delete-frame frame t)
    (cons (- (car loc) (car frame-loc))
          (- (cdr loc) (cdr frame-loc)))))

(defun move-frame-direction (amount direction &optional frame)
  (let* ((frame (or frame
                    (selected-frame)))
         (cpos (frame-position frame))
         (xchange
          (+
           (car frame-tools-move-offsets)
           (cl-case direction
             ('left
              (+ (car cpos) (- amount)))
             ('right (+ (car cpos) amount))
             (t (car cpos)))))
         (ychange
          (+
           (car frame-tools-move-offsets)
           (cl-case direction
             ('up (+ (cdr cpos) (- amount)))
             ('down (+ (cdr cpos) amount))
             (t (cdr cpos))))))
    (set-frame-position
     frame xchange ychange)))

(defhydra frame-tools-hydra (:color pink)
    ("l" frame-tools-move-fine-right "Right")
    ("h" frame-tools-move-fine-left "Left")
    ("k" frame-tools-move-fine-up "Up")
    ("j" frame-tools-move-fine-down "Down")
    ("L" frame-tools-move-coarse-right "Coarse Right")
    ("H" frame-tools-move-coarse-left "Coarse Left")
    ("K" frame-tools-move-coarse-up "Coarse Up")
    ("J" frame-tools-move-coarse-down "Coarse Down")
    ("q" nil "Quit"))

;;; Coarse movements
(defun frame-tools-move-coarse-up ()
  (interactive)
  (move-frame-direction frame-tools-move-coarse-distance 'up))

(defun frame-tools-move-coarse-down ()
  (interactive)
  (move-frame-direction frame-tools-move-coarse-distance 'down))

(defun frame-tools-move-coarse-left ()
  (interactive)
  (move-frame-direction frame-tools-move-coarse-distance 'left))

(defun frame-tools-move-coarse-right ()
  (interactive)
  (move-frame-direction frame-tools-move-coarse-distance 'right))

;;; Fine movements
(defun frame-tools-move-fine-up ()
  (interactive)
  (move-frame-direction frame-tools-move-fine-distance 'up))

(defun frame-tools-move-fine-down ()
  (interactive)
  (move-frame-direction frame-tools-move-fine-distance 'down))

(defun frame-tools-move-fine-left ()
  (interactive)
  (move-frame-direction frame-tools-move-fine-distance 'left))

(defun frame-tools-move-fine-right ()
  (interactive)
  (move-frame-direction frame-tools-move-fine-distance 'right))

(provide 'frame-tools)
