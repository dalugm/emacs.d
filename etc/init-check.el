;;; init-check.el --- checker for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Syntax and spelling check.
;;

;;; Code:

(use-package flymake
  :bind (("C-c ! b" . flymake-show-buffer-diagnostics)
         ("C-c ! p" . flymake-show-project-diagnostics)))

(use-package flyspell
  :defer t
  :config
  (cond
   ((executable-find "aspell")
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args
          '("--sug-mode=ultra" "--lang=en_US" "--camel-case")))))

(provide 'init-check)

;;; init-check.el ends here
