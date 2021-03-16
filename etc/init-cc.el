;;; init-cc.el --- C/C++ programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; C/C++ configuration.
;;

;;; Code:

;; do NOT use c-mode-common-hook or cc-mode-hook
;; because many major-modes use this hook
(defun my//cc-mode-common-hook-setup ()
  "Setup for the common cc-mode configuration."
  ;; https://stackoverflow.com/questions/3509919/emacs-c-opening-corresponding-header-file
  ;; Extend `ff-other-file-alist' if not find related files
  (local-set-key (kbd "C-c c o") #'ff-find-other-file))

(add-hook 'c-mode-common-hook #'my//cc-mode-common-hook-setup)

(provide 'init-cc)

;;; init-cc.el ends here
