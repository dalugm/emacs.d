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
  :hook (lsp-mode . lsp-ui-mode)
  :bind (("C-c a i" . lsp-ui-imenu)
         :map lsp-ui-mode-map
          ("C-c a a M-RET" . lsp-ui-sideline-apply-code-actions)
          ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
          ([remap xref-find-references] . lsp-ui-peek-find-references)))

(use-package lsp-treemacs
  :after (treemacs lsp))

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(provide 'init-lsp)

;;; init-lsp.el ends here
