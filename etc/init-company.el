;;; init-company.el --- company mode -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Auto-completion configuration.
;;

;;; Code:

(use-package company
  :hook (after-init . global-company-mode)
  :config
  ;; make previous/next selection in the popup cycles
  (setq company-selection-wrap-around t)
  ;; press `M-number' to choose candidate
  (setq company-show-numbers t)
  ;; make returned result case-sensitive
  (setq company-dabbrev-downcase nil)
  ;; align annotations to the right tooltip border
  (setq company-tooltip-align-annotations t)

  ;; NOT load company-mode for certain major modes.
  (setq company-global-modes
        '(not
           erc-mode
           eshell-mode
           gud-mode
           help-mode
           message-mode
           shell-mode
           )))

(use-package company-box
  :when (display-graphic-p)
  :hook (company-mode . company-box-mode))

;; Better sorting and filtering
(use-package company-prescient
  :hook (company-mode . company-prescient-mode))

(provide 'init-company)

;;; init-company.el ends here
