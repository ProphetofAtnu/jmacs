;; -*- lexical-binding: t; -*-
(require 'general)
(require 'cl-macs)

(defvar js/prefix-maps-list nil)

(defmacro define-prefix-map (name &rest defs)
  (let ((prefix-cmd (intern (format "prefix-%s-command" name)))
        (prefix-map (intern (format "prefix-%s-map" name))))
    (cl-pushnew prefix-map js/prefix-maps-list)
    `(general-define-key
      :prefix-command ',prefix-cmd
      :prefix-map ',prefix-map
      ,@defs)))

(defmacro mount-prefix-map (name ident)
  (let ((prefix-cmd (intern (format "prefix-%s-command" name)))
        (prefix-map (intern (format "prefix-%s-map" name))))
    `'(:prefix-command ,prefix-cmd
                       :prefix-map ,prefix-map
                       :wk ,ident)))

(provide 'prefix-binding-utils)
