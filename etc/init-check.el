;;; init-check.el --- checker for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Syntax and spelling check.
;;

;;; Code:

(use-package flymake
  :hook ((c-mode c++-mode) . flymake-mode)
  :bind (("C-c ! n" . flymake-goto-next-error)
         ("C-c ! p" . flymake-goto-prev-error)
         ("C-c ! d" . flymake-show-buffer-diagnostics)
         ("C-c ! D" . flymake-show-project-diagnostics)
         ("C-c ! s" . flymake-start)))

(use-package flyspell
  :when (executable-find "aspell")
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args
   '("--sug-mode=ultra" "--lang=en_US" "--camel-case")))

(provide 'init-check)

;;; init-check.el ends here
