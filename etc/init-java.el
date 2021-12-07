;;; init-java.el --- Java Programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Java Configurations.
;;

;;; Code:

(use-package kotlin-mode
  :mode "\\.kt\\'")

(use-package lsp-java
  :after (lsp-mode java-mode))

(provide 'init-java)

;;; init-java.el ends here
