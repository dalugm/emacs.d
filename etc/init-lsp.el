;;; init-lsp.el --- lsp configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Language Server Protocol (LSP) configuration.
;;

;;; Code:

(with-eval-after-load 'eglot
  ;; Eglot with volar.
  (add-to-list 'eglot-server-programs
               '(vue-mode . (eglot-volar "vue-language-server" "--stdio")))

  (defclass eglot-volar (eglot-lsp-server) ()
    :documentation "volar")

  (cl-defmethod eglot-initialization-options ((server eglot-volar))
    "Passes through required cquery initialization options"
    `(
      :typescript (:tsdk ,(expand-file-name "/usr/local/lib/node_modules/typescript/lib"))
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
                                      :defaultTagNameCase ""
                                      :defaultAttrNameCase ""
                                      :getDocumentNameCasesRequest :json-false
                                      :getDocumentSelectionRequest :json-false)
                         :schemaRequestService (
                                                :getDocumentContentRequest :json-false
                                                )
                         )
      :documentFeatures (
                         :selectionRange t,
                         :foldingRange :json-false,
                         :linkedEditingRange t,
                         :documentSymbol t,
                         :documentColor t,
                         :documentFormatting (
                                              :defaultPrintWidth 100
                                              :getDocumentPrintWidthRequest :json-false
                                              )
                         :defaultPrintWidth 100
                         :getDocumentPrintWidthRequest :json-false))))

(provide 'init-lsp)

;;; init-lsp.el ends here
