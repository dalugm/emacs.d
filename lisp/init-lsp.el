;;; init-lsp.el --- lsp configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Language Server Protocol (LSP) configuration.
;;

;;; Code:

(use-package eglot
  :bind (("C-c l l" . eglot)
         ("C-c l a" . eglot-code-actions)
         ("C-c l c" . eglot-show-workspace-configuration)
         ("C-c l f" . eglot-format)
         ("C-c l q" . eglot-shutdown)
         ("C-c l Q" . eglot-shutdown-all)
         ("C-c l r" . eglot-rename))
  :config
  ;; Vue with volar.
  (add-to-list 'eglot-server-programs
               '(vue-mode . (eglot-volar "vue-language-server" "--stdio")))

  (defclass eglot-volar (eglot-lsp-server) ()
    :documentation "volar")

  (cl-defmethod eglot-initialization-options ((server eglot-volar))
    "Pass through required cquery initialization options."
    `(
      :typescript (
                   :tsdk ,(expand-file-name
                           "lib"
                           (string-trim-right
                            (shell-command-to-string
                             "npm list --global --parseable typescript | head -n1")))
                   )
      :languageFeatures (
                         :references t
                         :implementation t
                         :definition t
                         :typeDefinition t
                         :rename t
                         :renameFileRefactoring t
                         :signatureHelp t
                         :codeAction t
                         :workspaceSymbol t
                         :completion (
                                      :defaultTagNameCase "both"
                                      :defaultAttrNameCase "kebabCase"
                                      :getDocumentNameCasesRequest nil
                                      :getDocumentSelectionRequest nil
                                      )
                         :schemaRequestService (
                                                :getDocumentContentRequest nil
                                                )
                         )
      :documentFeatures (
                         :selectionRange t
                         :foldingRange nil
                         :linkedEditingRange t
                         :documentSymbol t
                         :documentColor t
                         :documentFormatting (
                                              :defaultPrintWidth 100
                                              :getDocumentPrintWidthRequest nil
                                              )
                         :defaultPrintWidth 100
                         :getDocumentPrintWidthRequest nil))))

(provide 'init-lsp)

;;; init-lsp.el ends here
