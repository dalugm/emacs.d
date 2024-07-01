;;; init-vertico.el --- vertico configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  VERTical Interactive COmpletion.
;;

;;; Code:

(use-package vertico
  :hook (after-init . vertico-mode)
  :custom (vertico-cycle t))

;; Vertico sorts by history position.
(savehist-mode +1)

;; A few more useful configurations...
(use-package emacs
  :config
  (defun my--crm-indicator (args)
    "Add prompt indicator to `completing-read-multiple'.

We display [CRM<separator>], e.g., [CRM,] if the separator is a comma."
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'my--crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the
  ;; current mode.  Vertico commands are hidden in normal
  ;; buffers. This setting is useful beyond Vertico.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p))

(provide 'init-vertico)
;;; init-vertico.el ends here
