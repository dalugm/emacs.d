;;; init-js.el --- JavaScript programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  JavaScript configuration.
;;

;;; Code:

(defun my--js-mode-hook-setup ()
  "Default configuration for javascript."
  (setq js-indent-level 2))

(add-hook 'js-mode-hook #'my--js-mode-hook-setup)
(add-hook 'js-jsx-mode-hook #'my--js-mode-hook-setup)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :custom (typescript-indent-level 2))

(provide 'init-js)

;;; init-js.el ends here
