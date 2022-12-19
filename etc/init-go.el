;;; init-go.el --- GO Programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  GO Configuration.
;;

;;; Code:

(with-eval-after-load 'exec-path-from-shell
  (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))

(provide 'init-go)

;;; init-go.el ends here
