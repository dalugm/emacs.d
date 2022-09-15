;;; init-sh.el --- Shell programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Sh configuration.
;;

;;; Code:

(defun my--sh-mode-hook-setup ()
  "Default configuration for `sh-mode'."
  ;; default keybinding is overmapped by customized `smartparens-mode'.
  (define-key sh-mode-map (kbd "C-c )") #'sh-function))

(add-hook 'sh-mode-hook #'my--sh-mode-hook-setup)

(provide 'init-sh)

;;; init-sh.el ends here
