;;; init-python.el --- Python programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Python configuration.
;;

;;; Code:

(use-package python
  :mode ("\\.\\(c\\|i\\|r\\)?py\\'" . python-mode)
  :custom
  (python-indent-guess-indent-offset nil)
  (python-indent-offset 4))

(provide 'init-python)

;;; init-python.el ends here
