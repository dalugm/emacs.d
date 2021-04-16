;;; init-csharp.el --- C# programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  C# configuration.
;;

;;; Code:

(use-package csharp-mode
  :mode "\\.cs\\'")

(use-package web-mode
  :mode "\\.cshtml\\'")

(use-package sharper
  :bind ("C-c c n" . sharper-main-transient))

(provide 'init-csharp)

;;; init-csharp.el ends here
