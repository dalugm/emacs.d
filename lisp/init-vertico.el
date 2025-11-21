;;; init-vertico.el --- vertico configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  VERTical Interactive COmpletion.
;;

;;; Code:

(use-package vertico
  :custom
  (vertico-cycle t)
  :init (vertico-mode +1))

(use-package vertico-sort
  :after vertico
  :custom
  (vertico-sort-function 'vertico-sort-history-length-alpha))

;; Persist history over Emacs restarts for Vertico.
(use-package savehist :init (savehist-mode +1))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the
  ;; current mode.  Vertico commands are hidden in normal
  ;; buffers. This setting is useful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  (define-advice completing-read-multiple (:filter-args (args) crm-indicator)
    "Add prompt indicator to `completing-read-multiple'."
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))

  ;; Do not allow the cursor in the minibuffer prompt.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(provide 'init-vertico)
;;; init-vertico.el ends here
