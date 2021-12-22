;;; init-java.el --- Java Programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Java Configurations.
;;

;;; Code:

(use-package kotlin-mode
  :mode "\\.kt\\'")

(use-package groovy-mode
  :mode "\\.g\\(?:ant\\|roovy\\|radle\\)\\'")

(use-package lsp-java
  :after (:all lsp-mode (:any java-mode kotlin-mode groovy-mode)))

(provide 'init-java)

;;; init-java.el ends here
