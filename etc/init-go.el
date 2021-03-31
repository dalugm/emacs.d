;;; init-go.el --- GO Programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  GO Configuration.
;;

;;; Code:

(use-package go-mode
  :mode "\\.go\\'"
  :mode ("go\\.mod\\'" . go-dot-mod-mode)
  :config
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY"))))

(provide 'init-go)

;;; init-go.el ends here
