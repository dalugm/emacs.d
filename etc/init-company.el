;;; init-company.el --- company mode -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Auto-completion configuration.
;;

;;; Code:

(use-package company
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
          ("TAB" . company-complete-common-or-cycle)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("C-t" . company-other-backend))
  :config
  (setq company-idle-delay 0.2
        company-clang-insert-arguments nil
        company-require-match nil
        company-ctags-ignore-case t
        ;; I don't like the downcase word in company-dabbrev
        company-dabbrev-downcase nil
        ;; make previous/next selection in the popup cycles
        company-selection-wrap-around t
        ;; Some languages use camel case naming convention,
        ;; so company should be case sensitive.
        company-dabbrev-ignore-case nil
        ;; press `M-number' to choose candidate
        company-show-numbers t
        ;; https://github.com/company-mode/company-mode/issues/146
        company-tooltip-align-annotations t)

  ;; NOT load company-mode for certain major modes.
  ;; https://github.com/company-mode/company-mode/issues/29
  (setq company-global-modes
    '(not
       eshell-mode
       comint-mode
       erc-mode
       minibuffer-inactive-mode))

  ;; Press SPACE will accept the highlighted candidate and insert a space
  ;; `M-x describe-variable company-auto-complete-chars' for details.
  ;; So that's BAD idea.
  ;; `company-auto-complete' is obsolete since v0.9.14.
  (setq company-auto-commit nil)
  ;; can't work with TRAMP
  (setq company-backends (delete 'company-ropemacs company-backends)))

(use-package company-box
  :when (display-graphic-p)
  :hook (company-mode . company-box-mode)
  :init (setq company-box-doc-delay 0.3))

;; Better sorting and filtering
(use-package company-prescient
  :hook (company-mode . company-prescient-mode))

(provide 'init-company)

;;; init-company.el ends here
