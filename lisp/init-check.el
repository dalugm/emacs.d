;;; init-check.el --- checker for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Syntax and spelling check.
;;

;;; Code:

(use-package flymake
  :bind (("C-c ! b" . flymake-show-buffer-diagnostics)
         ("C-c ! p" . flymake-show-project-diagnostics))
  :custom (flymake-show-diagnostics-at-end-of-line 'short))

(use-package flyspell
  :defer t
  :config
  (when (executable-find "aspell")
    (setopt ispell-program-name "aspell"
            ispell-extra-args '("--sug-mode=ultra"
                                "--lang=en_US"
                                "--camel-case"))))

(provide 'init-check)
;;; init-check.el ends here
