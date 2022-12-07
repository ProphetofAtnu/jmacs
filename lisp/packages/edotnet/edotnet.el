;; -*- lexical-binding: t; -*-
(push default-directory load-path)

(require 'emacs-dotnet-serializers)

;; (benchmark 100000
;; 	   '(edn-serialize
;; 	     (emacs-dotnet-dt-completion-item
;; 	      :displayText "Blah Blah blah blah"
;; 	      :filterText "Blah Blah blah blah"
;; 	      :sortText "Blah Blah blah blah"
;; 	      :insertText "Blah Blah blah blah"
;; 	      :documentation "Blah blah")))

;; (edn-serialize
;; 	     (emacs-dotnet-dt-completion-item
;; 	      :displayText "Blah Blah blah blah"
;; 	      :filterText "Blah Blah blah blah"
;; 	      :sortText "Blah Blah blah blah"
;; 	      :insertText "Blah Blah blah blah"
;; 	      :documentation "Blah blah"))

(provide 'edotnet)
