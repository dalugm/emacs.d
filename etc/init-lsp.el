;;; init-lsp.el --- lsp configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Language Server Protocol (LSP) configuration.
;;

;;; Code:

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (defvar my-read-process-output-max (* 2 1024 1024)
    "Increase the amount of data which Emacs reads from the process.")
  (setq read-process-output-max my-read-process-output-max)
  :hook ((prog-mode . (lambda ()
                        (when (derived-mode-p 'c-mode 'c++-mode 'python-mode
                                              'sh-mode 'js-mode)
                          (lsp-deferred))))
         (lsp-mode . (lambda ()
                       ;; Integrate `which-key'
                       (lsp-enable-which-key-integration))))
  :bind (:map lsp-mode-map
          ([remap xref-find-definitions] . lsp-find-definition)
          ([remap xref-find-references]  . lsp-find-references))
  :config
  ;; `lsp-prefer-capf' is obsolete since v7.0.1
  (setq lsp-completion-provider t)
  ;; handle `yasnippet' by myself
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-indentation nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(provide 'init-lsp)

;;; init-lsp.el ends here
