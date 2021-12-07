;;; init-dart.el --- Dart Programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Dart Configurations.
;;

;;; Code:

(use-package dart-mode
  :mode "\\.dart\\'")

(use-package lsp-dart
  :after (lsp-mode dart-mode))

(provide 'init-dart)

;;; init-dart.el ends here
