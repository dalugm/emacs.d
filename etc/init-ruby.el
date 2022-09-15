;;; init-ruby.el --- Ruby programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Ruby configuration.
;;

;;; Code:

(defun my--ruby-mode-hook-setup ()
  "Default configuration for ruby."
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "RUBYPATH")))

(add-hook 'ruby-mode-hook #'my--ruby-mode-hook-setup)

(provide 'init-ruby)

;;; init-ruby.el ends here
