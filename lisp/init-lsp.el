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
         ("C-c l d" . eglot-find-declaration)
         ("C-c l f" . eglot-format)
         ("C-c l h" . eldoc)
         ("C-c l i" . eglot-find-implementation)
         ("C-c l n" . eglot-rename)
         ("C-c l q" . eglot-shutdown)
         ("C-c l t" . eglot-find-typeDefinition)
         ("C-c l Q" . eglot-shutdown-all)))

(use-package eglot-booster
  :when (executable-find "emacs-lsp-booster")
  :after eglot
  :custom (eglot-booster-io-only t)
  :config (eglot-booster-mode +1))

(use-package eglot-tempel
  :after (eglot tempel)
  :config (eglot-tempel-mode +1))

(provide 'init-lsp)
;;; init-lsp.el ends here
