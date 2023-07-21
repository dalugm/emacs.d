;;; init-company.el --- auto complete by company -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Auto complete by company.
;;

;;; Code:

(use-package company
  :hook (after-init . global-company-mode)
  :config
  ;; make previous/next selection in the popup cycles
  (setq company-selection-wrap-around t)
  ;; press `M-number' to choose candidate
  (setq company-show-quick-access t)
  ;; make returned result case-sensitive
  (setq company-dabbrev-downcase nil)
  ;; align annotations to the right tooltip border
  (setq company-tooltip-align-annotations t))

(use-package company-box
  :when (display-graphic-p)
  :hook (company-mode . company-box-mode))

(provide 'init-company)

;;; init-company.el ends here
