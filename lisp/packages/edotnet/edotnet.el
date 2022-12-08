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

(json-serialize
 (edn-serialize
  (emacs-dotnet-dt-request-completions 
   :code "This is a test"
   :linePosition (emacs-dotnet-dt-line-position
		  :line 0 :character 0)))
 :null-object nil)

(make-process
 :name "dni"
 :command '("dotnet" "interactive" "stdio")
 :connection-type 'pipe
 :buffer "*dni*")

(defun edn-make-comp-req ()
  (emacs-dotnet-dt-kernel-command-envelope :commandType "RequestCompletions"
					   :command
					   (emacs-dotnet-dt-request-completions 
					    :code (buffer-string)
					    :linePosition (emacs-dotnet-dt-line-position
							   :line (array-current-line) :character (current-column)))))
(process-send-string "dni" 
		     (concat (edn-make-command-string
			      (make-edn-commnd-struct :token "ASDF" :id "BSDF" :command-type "RequestCompletions"
						      :command '((code . "Console.W")
								 (linePosition 
								  (line . 0)
								  (character . 9))))) "\r\n"))
(benchmark 1
	   '(with-current-buffer "*dni*"
	     (mapcar
	      (lambda (ht) (gethash-mult 'ht 'displayText 'insertText))
	      (gethash
	       'completions
	       (edn-event-struct-event
		(edn-read-event-string
		 (buffer-string)))))))

(with-current-buffer "*dni*"
	     (mapcar
	      (lambda (ht) (gethash-mult 'ht 'displayText 'insertText))
	      (gethash
	       'completions
	       (edn-event-struct-event
		(edn-read-event-string
		 (buffer-string))))))
(provide 'edotnet)
