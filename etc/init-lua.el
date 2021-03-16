;;; init-lua.el --- Lua programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Lua configuration.
;;

;;; Code:

(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua")

(provide 'init-lua)

;;; init-lua.el ends here
