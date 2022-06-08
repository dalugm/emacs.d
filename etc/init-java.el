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

(provide 'init-java)

;;; init-java.el ends here
