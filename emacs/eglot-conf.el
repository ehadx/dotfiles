(require 'eglot)

(defcustom eglot-fsharp-fsautocomplete-args
  '(:automaticWorkspaceInit
    t
    :abstractClassStubGeneration t
	:abstractClassStubGenerationMethodBody
	"failwith \"Not Implemented\""
	:abstractClassStubGenerationObjectIdentifier "this"
	:addFsiWatcher nil
	:codeLenses (:references (:enabled t)
							 :signature (:enabled t))
	:disableFailedProjectNotifications nil
	:dotnetRoot ""
	:enableAdaptiveLspServer t
	:enableAnalyzers nil
	:enableMSBuildProjectGraph nil
	:enableReferenceCodeLens t
	:excludeProjectDirectories [".git" "paket-files" ".fable" "packages" "node_modules"]
	:externalAutocomplete nil
	:fsac (:attachDebugger nil
                           :cachedTypeCheckCount 200
				           :conserveMemory nil
				           :dotnetArgs nil
				           :netCoreDllPath ""
				           :parallelReferenceResolution nil
				           :silencedLogs nil)
	:fsiExtraParameters nil
	:fsiSdkFilePath ""
	:generateBinlog nil
	:indentationSize 4
	:inlayHints (:disableLongTooltip nil
								     :enabled t
								     :parameterNames t
								     :typeAnnotations t)
	:inlineValues (:enabled nil	:prefix "//")
	:interfaceStubGeneration t
	:interfaceStubGenerationMethodBody "failwith \"Not Implemented\""
	:interfaceStubGenerationObjectIdentifier "this"
	:keywordsAutocomplete t
	:lineLens (:enabled "replaceCodeLens" :prefix " // ")
	:linter t
	:pipelineHints (:enabled t :prefix " // ")
	:recordStubGeneration t
	:recordStubGenerationBody "failwith \"Not Implemented\""
	:resolveNamespaces t
	:saveOnSendLastSelection nil
	:simplifyNameAnalyzer t
	:smartIndent nil
	:suggestGitignore t
	:suggestSdkScripts t
	:unionCaseStubGeneration t
	:unionCaseStubGenerationBody "failwith \"Not Implemented\""
	:unusedDeclarationsAnalyzer t
	:unusedOpensAnalyzer t
	:verboseLogging nil
	:workspaceModePeekDeepLevel 4
	:workspacePath "")
  "Arguments for the fsautocomplete workspace configuration."
  :risky t
  )

(defclass eglot-fsautocomplete (eglot-lsp-server) ()
  :documentation "F# FsAutoComplete langserver.")

(cl-defmethod eglot-initialization-options ((_server eglot-fsautocomplete))
  "Passes through required FsAutoComplete initialization options."
  eglot-fsharp-fsautocomplete-args)

;; FIXME: this should be fixed in FsAutocomplete
(cl-defmethod xref-backend-definitions :around ((_type symbol) _identifier)
  "FsAutoComplete breaks spec and and returns error instead of empty list."
  (if (eq major-mode 'fsharp-mode)
      (condition-case err
          (cl-call-next-method)
        (jsonrpc-error
         (not (equal (cadddr err) '(jsonrpc-error-message . "Could not find declaration")))))
    (when (cl-next-method-p)
      (cl-call-next-method))))


(defun eglot-fsharp (interactive)
    (cons 'eglot-fsautocomplete `("dotnet" "fsautocomplete" "--adaptive-lsp-server-enabled")))


(defclass eglot-deno (eglot-lsp-server) ()
  :documentation "A custom class for deno lsp.")

(cl-defmethod eglot-initialization-options ((server eglot-deno))
  "Passes through required deno initialization options"
  (list :enable t
	    :lint t))

(use-package eglot
  :ensure t
  :config
    (add-hook 'java-mode-hook (lambda nil (eglot-ensure)))
    (add-hook 'python-mode-hook (lambda nil (eglot-ensure)))
    (add-hook 'csharp-mode-hook (lambda nil (eglot-ensure)))
    (add-hook 'rust-mode-hook (lambda nil (eglot-ensure)))
    (setq eglot-connect-timeout 60)
    (setq
     eglot-server-programs
     '((roc-ts-mode "roc_language_server")
       ((csharp-mode csharp-ts-mode) . ("OmniSharp" "-lsp"))
       ((fsharp-mode) . eglot-fsharp)
       ((rust-mode rust-ts-mode) . ("rust-analyzer"))
       ((js-mode typescript-mode) . (eglot-deno "deno" "lsp"))
       ((python-mode python-ts-mode) "pyright-langserver" "--stdio"))))
