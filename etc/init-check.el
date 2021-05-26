;;; init-check.el --- checker for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Syntax and spelling check.
;;

;;; Code:

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-global-modes
        '(not
           text-mode
           outline-mode
           fundamental-mode
           lisp-interaction-mode
           org-mode
           diff-mode
           shell-mode
           eshell-mode
           term-mode
           vterm-mode
           ))
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-indication-mode (if (display-graphic-p)
                                     'left-fringe
                                   'left-margin))
  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package flyspell
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args
   '("--sug-mode=ultra" "--lang=en_US" "--camel-case")))

(provide 'init-check)

;;; init-check.el ends here
