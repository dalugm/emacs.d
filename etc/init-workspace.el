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
  (setq winum-format "%s")
  (setq winum-mode-line-position 0)
  (winum-mode +1))

(use-package eyebrowse
  :hook (after-init . eyebrowse-mode)
  :custom (eyebrowse-keymap-prefix (kbd "C-c w"))
  :bind ("C-c w n" . eyebrowse-create-named-window-config))

(use-package find-file-in-project
  :when (executable-find "find")
  :bind (("C-c p a" . find-file-in-project-at-point)
         ("C-c p c" . find-file-in-current-directory-by-selected)
         ("C-c p d" . find-directory-in-project-by-selected)
         ("C-c p f" . ffip-create-project-file)
         ("C-c p i" . ffip-insert-file)
         ("C-c p p" . find-file-in-project-by-selected)
         ("C-c p r" . ffip-find-relative-path)
         ("C-c p s" . find-file-with-similar-name)
         ("C-c p v" . ffip-show-diff)
         (:map ffip-diff-mode-map
               ("a" . ffip-diff-apply-hunk)
               ("q" . ffip-diff-quit)))
  :config
  (when (eq system-type 'windows-nt)
    (setq ffip-find-executable "c:\\\\cygwin64\\\\bin\\\\find"))
  (when (executable-find "fd")
    (setq ffip-use-rust-fd t)))

(provide 'init-workspace)

;;; init-workspace.el ends here
