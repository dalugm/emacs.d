;;; init-python.el --- programming in python -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Python configuration.
;;

;;; Code:

(defun my//python-mode-hook-setup ()
  "Default configuration for python."
  ;; CamelCase aware editing operations
  (subword-mode +1)
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  (setq python-indent-offset 4)
  (setq python-shell-interpreter "python3")
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH")))

(add-hook 'python-mode-hook #'my//python-mode-hook-setup)

(use-package ein
  :disabled
  :defer t)

(provide 'init-python)

;;; init-python.el ends here
