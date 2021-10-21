;;; -*- lexical-binding: t; -*-


(defmacro +> (&rest forms)
  `(lambda (x) (-> x ,@forms)))

(provide 'core/utility)
