;; -*- lexical-binding: t; -*-


(defvar emacs-dotnet-kernel-command-types '(Cancel
                                            ChangeWorkingDirectory
                                            CompileProject
                                            DisplayError
                                            DisplayValue
                                            OpenDocument
                                            OpenProject
                                            Quit
                                            RequestCompletions
                                            RequestDiagnostics
                                            RequestHoverText
                                            RequestInput
                                            RequestKernelInfo
                                            RequestSignatureHelp
                                            RequestValue
                                            RequestValueInfos
                                            SendEditableCode
                                            SendValue
                                            SubmitCode
                                            UpdateDisplayedValue))

(defvar emacs-dotnet-kernel-event-types '(AssemblyProduced
                                          CodeSubmissionReceived
                                          CommandCancelled
                                          CommandFailed
                                          CommandSucceeded
                                          CompleteCodeSubmissionReceived
                                          CompletionsProduced
                                          DiagnosticLogEntryProduced
                                          DiagnosticsProduced
                                          DisplayedValueProduced
                                          DisplayedValueUpdated
                                          DocumentOpened
                                          ErrorProduced
                                          HoverTextProduced
                                          IncompleteCodeSubmissionReceived
                                          InputProduced
                                          KernelExtensionLoaded
                                          KernelInfoProduced
                                          KernelReady
                                          PackageAdded
                                          ProjectOpened
                                          ReturnValueProduced
                                          SignatureHelpProduced
                                          StandardErrorValueProduced
                                          StandardOutputValueProduced
                                          ValueInfosProduced
                                          ValueProduced
                                          WorkingDirectoryChanged))

(defconst
  emacs-dotnet-submission-types
  '(:run :diagnose))
(cl-deftype
    emacs-dotnet-submission-type
    ()
  '(or :run :diagnose))
(defconst
  emacs-dotnet-request-types
  '(:parse :serialize))
(cl-deftype
    emacs-dotnet-request-type
    ()
  '(or :parse :serialize))
(defconst
  emacs-dotnet-document-serialization-types
  '(:dib :ipynb))
(cl-deftype
    emacs-dotnet-document-serialization-type
    ()
  '(or :dib :ipynb))
(defconst
  emacs-dotnet-diagnostic-severities
  '(:hidden :info :warning :error))
(cl-deftype
    emacs-dotnet-diagnostic-severity
    ()
  '(or
    :hidden :info :warning :error))
(defconst
  emacs-dotnet-insert-text-formats
  '(:plaintext :snippet))
(cl-deftype
    emacs-dotnet-insert-text-format
    ()
  '(or :plaintext :snippet))

(defmacro emacs-dotnet-defclass (symbol &rest args)
  `(defclass
     ,symbol
     ,@args
     :allow-nil-initform t))

;;;; Data types
(emacs-dotnet-defclass
 emacs-dotnet-line-position
 ()
 ((line
   :initarg :line :type integer)
  (character
   :initarg :character :type integer)))

(emacs-dotnet-defclass
 emacs-dotnet-line-position-span
 ()
 ((start
   :initarg :start :type emacs-dotnet-line-position)
  (end
   :initarg :end :type emacs-dotnet-line-position)))

(emacs-dotnet-defclass
 emacs-dotnet-base64-encoded-assembly
 ()
 ((value
   :initarg :value :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-completion-item
 ()
 ((displayText
   :initarg :displayText :type string)
  (kind
   :initarg :kind :type string)
  (filterText
   :initarg :filterText :type string)
  (sortText
   :initarg :sortText :type string)
  (insertText
   :initarg :insertText :type string)
  (insertTextFormat
   :initarg :insertTextFormat :initform nil
   :type (or null emacs-dotnet-insert-text-format))
  (documentation
   :initarg :documentation :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-diagnostic
 ()
 ((linePositionSpan
   :initarg :linePositionSpan :type emacs-dotnet-line-position-span)
  (severity
   :initarg :severity :type symbol)
  (code
   :initarg :code :type string)
  (message
   :initarg :message :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-formatted-value
 ()
 ((mimeType
   :initarg :mimeType :type string)
  (value
   :initarg :value :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-interactive-document
 ()
 ((elements
   :initarg :elements :type (list
			     emacs-dotnet-interactive-document-element))
  (metadata
   :initarg :metadata :type hash-table)))

(emacs-dotnet-defclass
 emacs-dotnet-interactive-document-element
 ()
 ((id
   :initarg :id :initform nil
   :type (or null string))
  (kernelName
   :initarg :kernelName :initform nil
   :type (or null string))
  (contents
   :initarg :contents :type string)
  (outputs
   :initarg :outputs :type list)
  (executionOrder
   :initarg :executionOrder :type integer)
  (metadata
   :initarg :metadata :initform nil
   :type hash-table)))

(emacs-dotnet-defclass
 emacs-dotnet-kernel-info
 ()
 ((aliases
   :initarg :aliases :type (array string))
  (languageName
   :initarg :languageName :initform nil
   :type (or null string))
  (languageVersion
   :initarg :languageVersion :initform nil
   :type (or null string))
  (displayName
   :initarg :displayName :type string)
  (localName
   :initarg :localName :type string)
  (uri
   :initarg :uri :type string)
  (remoteUri
   :initarg :remoteUri :initform nil
   :type string)
  (supportedKernelCommands
   :initarg :supportedKernelCommands :type (array
					    emacs-dotnet-kernel-command-info))
  (supportedDirectives
   :initarg :supportedDirectives :type (array
					emacs-dotnet-kernel-directive-info))))

(emacs-dotnet-defclass
 emacs-dotnet-kernel-command-info
 ()
 ((name
   :initarg :name :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-kernel-directive-info
 ()
 ((name
   :initarg :name :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-kernel-value-info
 ()
 ((name
   :initarg :name :type string)
  (preferredMimeTypes
   :initarg :preferredMimeTypes :type (array string))))

(emacs-dotnet-defclass
 emacs-dotnet-project-file
 ()
 ((relativeFilePath
   :initarg :relativeFilePath :type string)
  (content
   :initarg :content :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-project
 ()
 ((files
   :initarg :files :type (array
			  emacs-dotnet-project-file))))

(emacs-dotnet-defclass
 emacs-dotnet-project-item
 ()
 ((relativeFilePath
   :initarg :relativeFilePath :type string)
  (regionNames
   :initarg :regionNames :type (array string))
  (regionsContent
   :initarg :regionsContent :type list)))

(emacs-dotnet-defclass
 emacs-dotnet-resolved-package-reference
 ()
 ((assemblyPaths
   :initarg :assemblyPaths :type (array string))
  (probingPaths
   :initarg :probingPaths :type (array string))
  (packageRoot
   :initarg :packageRoot :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-package-reference
 ()
 ((packageName
   :initarg :packageName :type string)
  (packageVersion
   :initarg :packageVersion :type string)
  (isPackageVersionSpecified
   :initarg :isPackageVersionSpecified :type boolean)))

(emacs-dotnet-defclass
 emacs-dotnet-signature-information
 ()
 ((label
   :initarg :label :type string)
  (documentation
   :initarg :documentation :type FormattedValue)
  (parameters
   :initarg :parameters :type (array
			       emacs-dotnet-parameter-information))))

(emacs-dotnet-defclass
 emacs-dotnet-parameter-information
 ()
 ((label
   :initarg :label :type string)
  (documentation
   :initarg :documentation :type FormattedValue)))

(emacs-dotnet-defclass
 emacs-dotnet-document-kernel-info-collection
 ()
 ((defaultKernelName
   :initarg :defaultKernelName :type string)
  (items
   :initarg :items :type ()
   document-kernel-info)))

(emacs-dotnet-defclass
 emacs-dotnet-kernel-command-envelope
 ()
 ((token
   :initarg :token :initform nil
   :type (or null string))
  (id
   :initarg :id :initform nil
   :type (or null string))
  (commandType
   :initarg :commandType :type symbol)
  (command :initarg :command)
  (routingSlip
   :initarg :routingSlip :initform nil
   :type (array string))))

(emacs-dotnet-defclass
 emacs-dotnet-kernel-event-envelope
 ()
 ((eventType
   :initarg :eventType :type string)
  (event :initarg :event)
  (command
   :initarg :command :initform nil
   :type (or null emacs-dotnet-kernel-command-envelope))
  (routingSlip
   :initarg :routingSlip :initform nil)))

;;;; Kernel commands 
(emacs-dotnet-defclass
 emacs-dotnet-kernel-command
 ()
 ((targetKernelName
   :initarg :targetKernelName :initform nil
   :type (or null string))
  (originUri
   :initarg :originUri :initform nil
   :type (or null string))
  (destinationUri
   :initarg :destinationUri :initform nil
   :type (or null string))))

(emacs-dotnet-defclass
 emacs-dotnet-language-service-command
 (emacs-dotnet-kernel-command)
 ((code
   :initarg :code :type string)
  (linePosition
   :initarg :linePosition :type emacs-dotnet-line-position)))

(emacs-dotnet-defclass
 emacs-dotnet-cancel
 (emacs-dotnet-kernel-command)
 ())

(emacs-dotnet-defclass
 emacs-dotnet-change-working-directory
 (emacs-dotnet-kernel-command)
 ((workingDirectory
   :initarg :workingDirectory :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-compile-project
 (emacs-dotnet-kernel-command)
 ())

(emacs-dotnet-defclass
 emacs-dotnet-display-error
 (emacs-dotnet-kernel-command)
 ((message
   :initarg :message :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-display-value
 (emacs-dotnet-kernel-command)
 ((formattedValue
   :initarg :formattedValue :type emacs-dotnet-formatted-value)
  (valueId
   :initarg :valueId :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-open-document
 (emacs-dotnet-kernel-command)
 ((relativeFilePath
   :initarg :relativeFilePath :type string)
  (regionName
   :initarg :regionName :initform nil
   :type (or null string))))

(emacs-dotnet-defclass
 emacs-dotnet-open-project
 (emacs-dotnet-kernel-command)
 ((project
   :initarg :project :type emacs-dotnet-project)))

(emacs-dotnet-defclass
 emacs-dotnet-quit
 (emacs-dotnet-kernel-command)
 ())


(emacs-dotnet-defclass
 emacs-dotnet-request-completions
 (emacs-dotnet-language-service-command)
 ())

(emacs-dotnet-defclass
 emacs-dotnet-request-diagnostics
 (emacs-dotnet-kernel-command)
 ((code
   :initarg :code :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-request-hover-text
 (emacs-dotnet-language-service-command)
 ())

(emacs-dotnet-defclass
 emacs-dotnet-request-input
 (emacs-dotnet-kernel-command)
 ((prompt
   :initarg :prompt :type string)
  (isPassword
   :initarg :isPassword :type boolean)
  (inputTypeHint
   :initarg :inputTypeHint :type string)
  (valueName
   :initarg :valueName :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-request-kernel-info
 (emacs-dotnet-kernel-command)
 ())

(emacs-dotnet-defclass
 emacs-dotnet-request-signature-help
 (emacs-dotnet-language-service-command)
 ())

(emacs-dotnet-defclass
 emacs-dotnet-request-value
 (emacs-dotnet-kernel-command)
 ((name
   :initarg :name :type string)
  (mimeType
   :initarg :mimeType :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-request-value-infos
 (emacs-dotnet-kernel-command)
 ())

(emacs-dotnet-defclass
 emacs-dotnet-send-editable-code
 (emacs-dotnet-kernel-command)
 ((kernelName
   :initarg :kernelName :type string)
  (code
   :initarg :code :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-send-value
 (emacs-dotnet-kernel-command)
 ((formattedValue
   :initarg :formattedValue :type emacs-dotnet-formatted-value)
  (name
   :initarg :name :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-submit-code
 (emacs-dotnet-kernel-command)
 ((code
   :initarg :code :type string)
  (submissionType
   :initarg :submissionType :initform nil
   :type (or null emacs-dotnet-submission-type))))

(emacs-dotnet-defclass
 emacs-dotnet-update-displayed-value
 (emacs-dotnet-kernel-command)
 ((formattedValue
   :initarg :formattedValue :type emacs-dotnet-formatted-value)
  (valueId
   :initarg :valueId :type string)))

;;;; Kernel Events
(emacs-dotnet-defclass
 emacs-dotnet-kernel-event
 ()
 ())

(emacs-dotnet-defclass
 emacs-dotnet-interactive-document-output-element
 ()
 ())

(emacs-dotnet-defclass
 emacs-dotnet-notebook-parser-server-response
 ()
 ((id :initarg :id :type string)))


(emacs-dotnet-defclass
 emacs-dotnet-display-element
 (emacs-dotnet-interactive-document-output-element)
 ((data
   :initarg :data :type 'hash-table)
  (metadata
   :initarg :metadata :type 'hash-table)))

(emacs-dotnet-defclass
 emacs-dotnet-return-value-element
 ()
 ((data
   :initarg :data :type 'hash-table)
  (executionOrder
   :initarg :executionOrder :type integer)
  (metadata
   :initarg :metadata :type 'hash-table)))

(emacs-dotnet-defclass
 emacs-dotnet-text-element
 ()
 ((name
   :initarg :name :type string)
  (text
   :initarg :text :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-error-element
 ()
 ((errorName
   :initarg :errorName :type string)
  (errorValue
   :initarg :errorValue :type string)
  (stackTrace
   :initarg :stackTrace :type (array string))))

(emacs-dotnet-defclass
 emacs-dotnet-document-kernel-info
 ()
 ((name
   :initarg :name :type string)
  (languageName
   :initarg :languageName :initform nil
   :type (or null string))
  (aliases
   :initarg :aliases :type (array string))))

(emacs-dotnet-defclass
 emacs-dotnet-notebook-parse-or-serialize-request
 ()
 ((type
   :initarg :type :type emacs-dotnet-request-type)
  (id :initarg :id :type string)
  (serializationType
   :initarg :serializationType :type emacs-dotnet-document-serialization-type)
  (defaultLanguage
   :initarg :defaultLanguage :type string)))


(emacs-dotnet-defclass
 emacs-dotnet-notebook-parse-request
 (emacs-dotnet-notebook-parse-or-serialize-request)
 ((type
   :initarg :type :type emacs-dotnet-request-type)
  (rawData
   :initarg :rawData :type array)))

(emacs-dotnet-defclass
 emacs-dotnet-notebook-serialize-request
 (emacs-dotnet-notebook-parse-or-serialize-request)
 ((type
   :initarg :type :type emacs-dotnet-request-type)
  (newLine
   :initarg :newLine :type string)
  (document
   :initarg :document :type emacs-dotnet-interactive-document)))

(emacs-dotnet-defclass
 emacs-dotnet-notebook-parse-response
 (emacs-dotnet-notebook-parser-server-response)
 ((document
   :initarg :document :type emacs-dotnet-interactive-document)))

(emacs-dotnet-defclass
 emacs-dotnet-notebook-serialize-response
 (emacs-dotnet-notebook-parser-server-response)
 ((rawData
   :initarg :rawData :type array)))

(emacs-dotnet-defclass
 emacs-dotnet-notebook-error-response
 (emacs-dotnet-notebook-parser-server-response)
 ((errorMessage
   :initarg :errorMessage :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-assembly-produced
 (emacs-dotnet-kernel-event)
 ((assembly
   :initarg :assembly :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-code-submission-received
 (emacs-dotnet-kernel-event)
 ((code
   :initarg :code :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-command-cancelled
 (emacs-dotnet-kernel-event)
 ())

(emacs-dotnet-defclass
 emacs-dotnet-kernel-command-completion-event
 (emacs-dotnet-kernel-event)
 ((executionOrder
   :initarg :executionOrder :initform nil
   :type (or null integer))))

(emacs-dotnet-defclass
 emacs-dotnet-command-failed
 (emacs-dotnet-kernel-command-completion-event)
 ((message
   :initarg :message :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-command-succeeded
 (emacs-dotnet-kernel-command-completion-event)
 ())

(emacs-dotnet-defclass
 emacs-dotnet-complete-code-submission-received
 (emacs-dotnet-kernel-event)
 ((code
   :initarg :code :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-completions-produced
 (emacs-dotnet-kernel-event)
 ((linePositionSpan
   :initarg :linePositionSpan :initform nil
   :type (or null emacs-dotnet-line-position-span))
  (completions
   :initarg :completions :type (array
				emacs-dotnet-completion-item))))

(emacs-dotnet-defclass
 emacs-dotnet-diagnostic-event
 (emacs-dotnet-kernel-event)
 ())

(emacs-dotnet-defclass
 emacs-dotnet-diagnostic-log-entry-produced
 (emacs-dotnet-diagnostic-event)
 ((message
   :initarg :message :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-diagnostics-produced
 (emacs-dotnet-kernel-event)
 ((diagnostics
   :initarg :diagnostics :type (array Diagnostic))
  (formattedDiagnostics
   :initarg :formattedDiagnostics :type (array
					 emacs-dotnet-formatted-value))))

(emacs-dotnet-defclass
 emacs-dotnet-display-event
 (emacs-dotnet-kernel-event)
 ((formattedValues
   :initarg :formattedValues :type (array
				    emacs-dotnet-formatted-value))
  (valueId
   :initarg :valueId :initform nil
   :type (or null string))))

(emacs-dotnet-defclass
 emacs-dotnet-displayed-value-produced
 (emacs-dotnet-display-event)
 ())

(emacs-dotnet-defclass
 emacs-dotnet-displayed-value-updated
 (emacs-dotnet-display-event)
 ())

(emacs-dotnet-defclass
 emacs-dotnet-document-opened
 (emacs-dotnet-kernel-event)
 ((relativeFilePath
   :initarg :relativeFilePath :type string)
  (regionName
   :initarg :regionName :initform nil
   :type (or null string))
  (content
   :initarg :content :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-error-produced
 (emacs-dotnet-display-event)
 ((message
   :initarg :message :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-hover-text-produced
 (emacs-dotnet-kernel-event)
 ((content
   :initarg :content :type (array
			    emacs-dotnet-formatted-value))
  (linePositionSpan
   :initarg :linePositionSpan :initform nil
   :type (or null emacs-dotnet-line-position-span))))

(emacs-dotnet-defclass
 emacs-dotnet-incomplete-code-submission-received
 (emacs-dotnet-kernel-event)
 ())

(emacs-dotnet-defclass
 emacs-dotnet-input-produced
 (emacs-dotnet-kernel-event)
 ((value
   :initarg :value :type string)))

(emacs-dotnet-defclass
 emacs-dotnet-kernel-extension-loaded
 (emacs-dotnet-kernel-event)
 ())

(emacs-dotnet-defclass
 emacs-dotnet-kernel-info-produced
 (emacs-dotnet-kernel-event)
 ((kernelInfo
   :initarg :kernelInfo :type emacs-dotnet-kernel-info)))

(emacs-dotnet-defclass
 emacs-dotnet-kernel-ready
 (emacs-dotnet-kernel-event)
 ())

(emacs-dotnet-defclass
 emacs-dotnet-package-added
 (emacs-dotnet-kernel-event)
 ((packageReference
   :initarg :packageReference :type emacs-dotnet-resolved-package-reference)))

(emacs-dotnet-defclass
 emacs-dotnet-project-opened
 (emacs-dotnet-kernel-event)
 ((projectItems
   :initarg :projectItems :type (array
				 emacs-dotnet-project-item))))

(emacs-dotnet-defclass
 emacs-dotnet-return-value-produced
 (emacs-dotnet-display-event)
 ())

(emacs-dotnet-defclass
 emacs-dotnet-signature-help-produced
 (emacs-dotnet-kernel-event)
 ((signatures
   :initarg :signatures :type (array
			       emacs-dotnet-signature-information))
  (activeSignatureIndex
   :initarg :activeSignatureIndex :type integer)
  (activeParameterIndex
   :initarg :activeParameterIndex :type integer)))

(emacs-dotnet-defclass
 emacs-dotnet-standard-error-value-produced
 (emacs-dotnet-display-event)
 ())

(emacs-dotnet-defclass
 emacs-dotnet-standard-output-value-produced
 (emacs-dotnet-display-event)
 ())

(emacs-dotnet-defclass
 emacs-dotnet-value-infos-produced
 (emacs-dotnet-kernel-event)
 ((valueInfos
   :initarg :valueInfos :type (array
			       emacs-dotnet-kernel-value-info))))

(emacs-dotnet-defclass
 emacs-dotnet-value-produced
 (emacs-dotnet-kernel-event)
 ((name
   :initarg :name :type string)
  (formattedValue
   :initarg :formattedValue :type emacs-dotnet-formatted-value)))

(emacs-dotnet-defclass
 emacs-dotnet-working-directory-changed
 (emacs-dotnet-kernel-event)
 ((workingDirectory
   :initarg :workingDirectory :type string)))

(provide 'emacs-dotnet-contracts)

;; Local Variables:
;; read-symbol-shorthands: (("edn-" . "emacs-dotnet-"))
;; End:
