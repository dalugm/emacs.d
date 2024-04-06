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
         ("C-c l r" . eglot-rename)))

(use-package eglot-booster
  :when (executable-find "emacs-lsp-booster")
  :after eglot
  :config (eglot-booster-mode +1))

(use-package eglot-tempel
  :after (eglot tempel)
  :config (eglot-tempel-mode +1))

(provide 'init-lsp)
;;; init-lsp.el ends here
