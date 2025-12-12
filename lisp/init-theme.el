;;; init-theme.el --- Theme for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Theme configuration.
;;

;;; Code:

(use-package modus-themes)

(use-package ef-themes :after modus-themes)

(use-package standard-themes :after modus-themes)

(use-package sinolor-themes :after modus-themes)

(use-package catppuccin-theme)

(use-package color-theme-sanityinc-tomorrow)

(use-package color-theme-sanityinc-solarized)

(use-package monokai-theme)

(use-package tao-theme)

(use-package zenburn-theme)

(provide 'init-theme)
;;; init-theme.el ends here
