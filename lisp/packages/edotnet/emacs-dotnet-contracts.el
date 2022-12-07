;; -*- lexical-binding: t; eieio-backward-compatibility: nil  -*-

(setq-local eieio-backward-compatibility nil)

(defvar emacs-dotnet-dt-kernel-command-types '(Cancel
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

(defvar emacs-dotnet-dt-kernel-event-types '(AssemblyProduced
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

(defvar emacs-dotnet-dt-classes nil)

(defconst
  emacs-dotnet-dt-submission-types
  '(:run :diagnose))
(cl-deftype
    emacs-dotnet-dt-submission-type
    ()
  '(or :run :diagnose))
(defconst
  emacs-dotnet-dt-request-types
  '(:parse :serialize))
(cl-deftype
    emacs-dotnet-dt-request-type
    ()
  '(or :parse :serialize))
(defconst
  emacs-dotnet-dt-document-serialization-types
  '(:dib :ipynb))
(cl-deftype
    emacs-dotnet-dt-document-serialization-type
    ()
  '(or :dib :ipynb))
(defconst
  emacs-dotnet-dt-diagnostic-severities
  '(:hidden :info :warning :error))
(cl-deftype
    emacs-dotnet-dt-diagnostic-severity
    ()
  '(or
    :hidden :info :warning :error))
(defconst
  emacs-dotnet-dt-insert-text-formats
  '(:plaintext :snippet))
(cl-deftype
    emacs-dotnet-dt-insert-text-format
    ()
  '(or :plaintext :snippet))

(cl-deftype emacs-dotnet-dt-listof (_) '(satisfies listp))

(defmacro emacs-dotnet-dt-defclass (symbol parent slots &rest args)
  `(progn
    (cl-pushnew ',symbol emacs-dotnet-dt-classes)
    (defclass
      ,symbol
      (,@parent)
      ,slots
      :allow-nil-initform t)))

;;;; Data types
(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-line-position
 ()
 ((line
   :initarg :line :type integer)
  (character
   :initarg :character :type integer)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-line-position-span
 ()
 ((start
   :initarg :start :type emacs-dotnet-dt-line-position)
  (end
   :initarg :end :type emacs-dotnet-dt-line-position)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-base64-encoded-assembly
 ()
 ((value
   :initarg :value :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-completion-item
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
   :type (or null emacs-dotnet-dt-insert-text-format))
  (documentation
   :initarg :documentation :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-diagnostic
 ()
 ((linePositionSpan
   :initarg :linePositionSpan :type emacs-dotnet-dt-line-position-span)
  (severity
   :initarg :severity :type symbol)
  (code
   :initarg :code :type string)
  (message
   :initarg :message :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-formatted-value
 ()
 ((mimeType
   :initarg :mimeType :type string)
  (value
   :initarg :value :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-interactive-document
 ()
 ((elements
   :initarg :elements :type (emacs-dotnet-dt-listof
			     emacs-dotnet-dt-interactive-document-element
			     ))
  (metadata
   :initarg :metadata :type hash-table)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-interactive-document-element
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

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-kernel-info
 ()
 ((aliases
   :initarg :aliases :type (emacs-dotnet-dt-listof string))
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
   :initarg :supportedKernelCommands :type (emacs-dotnet-dt-listof
					    emacs-dotnet-dt-kernel-command-info))
  (supportedDirectives
   :initarg :supportedDirectives :type (emacs-dotnet-dt-listof
					emacs-dotnet-dt-kernel-directive-info))))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-kernel-command-info
 ()
 ((name
   :initarg :name :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-kernel-directive-info
 ()
 ((name
   :initarg :name :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-kernel-value-info
 ()
 ((name
   :initarg :name :type string)
  (preferredMimeTypes
   :initarg :preferredMimeTypes :type (emacs-dotnet-dt-listof string))))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-project-file
 ()
 ((relativeFilePath
   :initarg :relativeFilePath :type string)
  (content
   :initarg :content :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-project
 ()
 ((files
   :initarg :files :type (emacs-dotnet-dt-listof
			  emacs-dotnet-dt-project-file))))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-project-item
 ()
 ((relativeFilePath
   :initarg :relativeFilePath :type string)
  (regionNames
   :initarg :regionNames :type (emacs-dotnet-dt-listof string))
  (regionsContent
   :initarg :regionsContent :type list)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-resolved-package-reference
 ()
 ((assemblyPaths
   :initarg :assemblyPaths :type (emacs-dotnet-dt-listof string))
  (probingPaths
   :initarg :probingPaths :type (emacs-dotnet-dt-listof string))
  (packageRoot
   :initarg :packageRoot :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-package-reference
 ()
 ((packageName
   :initarg :packageName :type string)
  (packageVersion
   :initarg :packageVersion :type string)
  (isPackageVersionSpecified
   :initarg :isPackageVersionSpecified :type boolean)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-signature-information
 ()
 ((label
   :initarg :label :type string)
  (documentation
   :initarg :documentation :type FormattedValue)
  (parameters
   :initarg :parameters :type (emacs-dotnet-dt-listof
			       emacs-dotnet-dt-parameter-information))))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-parameter-information
 ()
 ((label
   :initarg :label :type string)
  (documentation
   :initarg :documentation :type FormattedValue)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-document-kernel-info-collection
 ()
 ((defaultKernelName
   :initarg :defaultKernelName :type string)
  (items
   :initarg :items :type ()
   document-kernel-info)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-kernel-command-envelope
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
   :type (emacs-dotnet-dt-listof string))))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-kernel-event-envelope
 ()
 ((eventType
   :initarg :eventType :type string)
  (event :initarg :event)
  (command
   :initarg :command :initform nil
   :type (or null emacs-dotnet-dt-kernel-command-envelope))
  (routingSlip
   :initarg :routingSlip :initform nil)))

;;;; Kernel commands 
(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-kernel-command
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

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-language-service-command
 (emacs-dotnet-dt-kernel-command)
 ((code
   :initarg :code :type string)
  (linePosition
   :initarg :linePosition :type emacs-dotnet-dt-line-position)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-cancel
 (emacs-dotnet-dt-kernel-command)
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-change-working-directory
 (emacs-dotnet-dt-kernel-command)
 ((workingDirectory
   :initarg :workingDirectory :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-compile-project
 (emacs-dotnet-dt-kernel-command)
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-display-error
 (emacs-dotnet-dt-kernel-command)
 ((message
   :initarg :message :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-display-value
 (emacs-dotnet-dt-kernel-command)
 ((formattedValue
   :initarg :formattedValue :type emacs-dotnet-dt-formatted-value)
  (valueId
   :initarg :valueId :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-open-document
 (emacs-dotnet-dt-kernel-command)
 ((relativeFilePath
   :initarg :relativeFilePath :type string)
  (regionName
   :initarg :regionName :initform nil
   :type (or null string))))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-open-project
 (emacs-dotnet-dt-kernel-command)
 ((project
   :initarg :project :type emacs-dotnet-dt-project)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-quit
 (emacs-dotnet-dt-kernel-command)
 ())


(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-request-completions
 (emacs-dotnet-dt-language-service-command)
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-request-diagnostics
 (emacs-dotnet-dt-kernel-command)
 ((code
   :initarg :code :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-request-hover-text
 (emacs-dotnet-dt-language-service-command)
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-request-input
 (emacs-dotnet-dt-kernel-command)
 ((prompt
   :initarg :prompt :type string)
  (isPassword
   :initarg :isPassword :type boolean)
  (inputTypeHint
   :initarg :inputTypeHint :type string)
  (valueName
   :initarg :valueName :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-request-kernel-info
 (emacs-dotnet-dt-kernel-command)
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-request-signature-help
 (emacs-dotnet-dt-language-service-command)
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-request-value
 (emacs-dotnet-dt-kernel-command)
 ((name
   :initarg :name :type string)
  (mimeType
   :initarg :mimeType :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-request-value-infos
 (emacs-dotnet-dt-kernel-command)
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-send-editable-code
 (emacs-dotnet-dt-kernel-command)
 ((kernelName
   :initarg :kernelName :type string)
  (code
   :initarg :code :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-send-value
 (emacs-dotnet-dt-kernel-command)
 ((formattedValue
   :initarg :formattedValue :type emacs-dotnet-dt-formatted-value)
  (name
   :initarg :name :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-submit-code
 (emacs-dotnet-dt-kernel-command)
 ((code
   :initarg :code :type string)
  (submissionType
   :initarg :submissionType :initform nil
   :type (or null emacs-dotnet-dt-submission-type))))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-update-displayed-value
 (emacs-dotnet-dt-kernel-command)
 ((formattedValue
   :initarg :formattedValue :type emacs-dotnet-dt-formatted-value)
  (valueId
   :initarg :valueId :type string)))

;;;; Kernel Events
(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-kernel-event
 ()
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-interactive-document-output-element
 ()
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-notebook-parser-server-response
 ()
 ((id :initarg :id :type string)))


(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-display-element
 (emacs-dotnet-dt-interactive-document-output-element)
 ((data
   :initarg :data :type 'hash-table)
  (metadata
   :initarg :metadata :type 'hash-table)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-return-value-element
 ()
 ((data
   :initarg :data :type 'hash-table)
  (executionOrder
   :initarg :executionOrder :type integer)
  (metadata
   :initarg :metadata :type 'hash-table)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-text-element
 ()
 ((name
   :initarg :name :type string)
  (text
   :initarg :text :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-error-element
 ()
 ((errorName
   :initarg :errorName :type string)
  (errorValue
   :initarg :errorValue :type string)
  (stackTrace
   :initarg :stackTrace :type (emacs-dotnet-dt-listof string))))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-document-kernel-info
 ()
 ((name
   :initarg :name :type string)
  (languageName
   :initarg :languageName :initform nil
   :type (or null string))
  (aliases
   :initarg :aliases :type (emacs-dotnet-dt-listof string))))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-notebook-parse-or-serialize-request
 ()
 ((type
   :initarg :type :type emacs-dotnet-dt-request-type)
  (id :initarg :id :type string)
  (serializationType
   :initarg :serializationType :type emacs-dotnet-dt-document-serialization-type)
  (defaultLanguage
   :initarg :defaultLanguage :type string)))


(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-notebook-parse-request
 (emacs-dotnet-dt-notebook-parse-or-serialize-request)
 ((type
   :initarg :type :type emacs-dotnet-dt-request-type)
  (rawData
   :initarg :rawData :type array)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-notebook-serialize-request
 (emacs-dotnet-dt-notebook-parse-or-serialize-request)
 ((type
   :initarg :type :type emacs-dotnet-dt-request-type)
  (newLine
   :initarg :newLine :type string)
  (document
   :initarg :document :type emacs-dotnet-dt-interactive-document)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-notebook-parse-response
 (emacs-dotnet-dt-notebook-parser-server-response)
 ((document
   :initarg :document :type emacs-dotnet-dt-interactive-document)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-notebook-serialize-response
 (emacs-dotnet-dt-notebook-parser-server-response)
 ((rawData
   :initarg :rawData :type array)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-notebook-error-response
 (emacs-dotnet-dt-notebook-parser-server-response)
 ((errorMessage
   :initarg :errorMessage :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-assembly-produced
 (emacs-dotnet-dt-kernel-event)
 ((assembly
   :initarg :assembly :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-code-submission-received
 (emacs-dotnet-dt-kernel-event)
 ((code
   :initarg :code :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-command-cancelled
 (emacs-dotnet-dt-kernel-event)
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-kernel-command-completion-event
 (emacs-dotnet-dt-kernel-event)
 ((executionOrder
   :initarg :executionOrder :initform nil
   :type (or null integer))))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-command-failed
 (emacs-dotnet-dt-kernel-command-completion-event)
 ((message
   :initarg :message :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-command-succeeded
 (emacs-dotnet-dt-kernel-command-completion-event)
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-complete-code-submission-received
 (emacs-dotnet-dt-kernel-event)
 ((code
   :initarg :code :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-completions-produced
 (emacs-dotnet-dt-kernel-event)
 ((linePositionSpan
   :initarg :linePositionSpan :initform nil
   :type (or null emacs-dotnet-dt-line-position-span))
  (completions
   :initarg :completions :type (emacs-dotnet-dt-listof
				emacs-dotnet-dt-completion-item))))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-diagnostic-event
 (emacs-dotnet-dt-kernel-event)
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-diagnostic-log-entry-produced
 (emacs-dotnet-dt-diagnostic-event)
 ((message
   :initarg :message :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-diagnostics-produced
 (emacs-dotnet-dt-kernel-event)
 ((diagnostics
   :initarg :diagnostics :type (emacs-dotnet-dt-listof Diagnostic))
  (formattedDiagnostics
   :initarg :formattedDiagnostics :type (emacs-dotnet-dt-listof
					 emacs-dotnet-dt-formatted-value))))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-display-event
 (emacs-dotnet-dt-kernel-event)
 ((formattedValues
   :initarg :formattedValues :type (emacs-dotnet-dt-listof
				    emacs-dotnet-dt-formatted-value))
  (valueId
   :initarg :valueId :initform nil
   :type (or null string))))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-displayed-value-produced
 (emacs-dotnet-dt-display-event)
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-displayed-value-updated
 (emacs-dotnet-dt-display-event)
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-document-opened
 (emacs-dotnet-dt-kernel-event)
 ((relativeFilePath
   :initarg :relativeFilePath :type string)
  (regionName
   :initarg :regionName :initform nil
   :type (or null string))
  (content
   :initarg :content :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-error-produced
 (emacs-dotnet-dt-display-event)
 ((message
   :initarg :message :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-hover-text-produced
 (emacs-dotnet-dt-kernel-event)
 ((content
   :initarg :content :type (emacs-dotnet-dt-listof
			    emacs-dotnet-dt-formatted-value))
  (linePositionSpan
   :initarg :linePositionSpan :initform nil
   :type (or null emacs-dotnet-dt-line-position-span))))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-incomplete-code-submission-received
 (emacs-dotnet-dt-kernel-event)
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-input-produced
 (emacs-dotnet-dt-kernel-event)
 ((value
   :initarg :value :type string)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-kernel-extension-loaded
 (emacs-dotnet-dt-kernel-event)
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-kernel-info-produced
 (emacs-dotnet-dt-kernel-event)
 ((kernelInfo
   :initarg :kernelInfo :type emacs-dotnet-dt-kernel-info)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-kernel-ready
 (emacs-dotnet-dt-kernel-event)
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-package-added
 (emacs-dotnet-dt-kernel-event)
 ((packageReference
   :initarg :packageReference :type emacs-dotnet-dt-resolved-package-reference)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-project-opened
 (emacs-dotnet-dt-kernel-event)
 ((projectItems
   :initarg :projectItems :type (emacs-dotnet-dt-listof
				 emacs-dotnet-dt-project-item))))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-return-value-produced
 (emacs-dotnet-dt-display-event)
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-signature-help-produced
 (emacs-dotnet-dt-kernel-event)
 ((signatures
   :initarg :signatures :type (emacs-dotnet-dt-listof
			       emacs-dotnet-dt-signature-information))
  (activeSignatureIndex
   :initarg :activeSignatureIndex :type integer)
  (activeParameterIndex
   :initarg :activeParameterIndex :type integer)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-standard-error-value-produced
 (emacs-dotnet-dt-display-event)
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-standard-output-value-produced
 (emacs-dotnet-dt-display-event)
 ())

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-value-infos-produced
 (emacs-dotnet-dt-kernel-event)
 ((valueInfos
   :initarg :valueInfos :type (emacs-dotnet-dt-listof
			       emacs-dotnet-dt-kernel-value-info))))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-value-produced
 (emacs-dotnet-dt-kernel-event)
 ((name
   :initarg :name :type string)
  (formattedValue
   :initarg :formattedValue :type emacs-dotnet-dt-formatted-value)))

(emacs-dotnet-dt-defclass
 emacs-dotnet-dt-working-directory-changed
 (emacs-dotnet-dt-kernel-event)
 ((workingDirectory
   :initarg :workingDirectory :type string)))

(provide 'emacs-dotnet-contracts)

;; Local Variables:
;; read-symbol-shorthands: (("edn-" . "emacs-dotnet-dt-"))
;; End:
