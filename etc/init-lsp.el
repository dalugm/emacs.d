;;; init-lsp.el --- lsp configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Language Server Protocol (LSP) configuration.
;;

;;; Code:

(use-package lsp-mode
  :bind ("C-c l l" . lsp)
  :init (setq lsp-keymap-prefix "C-c a")
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  ;; NOTE: Only take effects after Emacs 27
  (when (boundp 'read-process-output-max)
    (setq read-process-output-max (* 2 1024 1024)))
  :hook (lsp-mode . (lambda ()
                      ;; integrate with `which-key'
                      (when (featurep 'which-key)
                        (lsp-enable-which-key-integration))))
  :bind (:map lsp-mode-map
          ([remap xref-find-definitions] . lsp-find-definition)
          ([remap xref-find-references]  . lsp-find-references))
  :config
  ;; avoid visual interference
  (setq lsp-enable-symbol-highlighting nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(provide 'init-lsp)

;;; init-lsp.el ends here
