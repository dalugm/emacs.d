;;; init-workspace.el --- workspace in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Separate workspace in Emacs.
;;

;;; Code:

(use-package ace-window
  :bind (([remap other-window] . ace-window)
         ("C-c w s" . ace-swap-window)
         ("C-c w d" . ace-delete-window)
         ("C-c w o" . ace-delete-other-windows))
  :config
  ;; Inherits from `avy'
  (setq aw-keys avy-keys)
  (setq aw-background avy-background))

(use-package winum
  :config
  (setq winum-format "%s ")
  (setq winum-mode-line-position 0)
  (winum-mode +1))

(use-package eyebrowse
  :hook (after-init . eyebrowse-mode)
  :custom (eyebrowse-keymap-prefix (kbd "C-c w"))
  :bind ("C-c w n" . eyebrowse-create-named-window-config))

(provide 'init-workspace)

;;; init-workspace.el ends here
