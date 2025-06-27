;;; init-vc.el --- version control -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Version control system.
;;

;;; Code:

;; Visit version controlled symlink without asking.
(setopt vc-follow-symlinks t)

(use-package magit
  :bind (("C-x g"   . magit-status)
         ("C-c v g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :config
  ;; Add module section into the status buffer.
  (magit-add-section-hook 'magit-status-sections-hook
                          #'magit-insert-modules
                          #'magit-insert-stashes #'append))

(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (after-init . diff-hl-flydiff-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-margin-symbols-alist '((insert . "+") (delete . "-")
                                  (change . "=") (ignored . "!")
                                  (unknown . "?"))))

(use-package git-modes
  :mode ("/\\.dockerignore\\'" . gitignore-mode))

(use-package git-link
  :custom (git-link-use-commit t)
  :bind (("C-c v l" . git-link)
         ("C-c v c" . git-link-commit)
         ("C-c v d" . git-link-dispatch)
         ("C-c v h" . git-link-homepage)))

(use-package git-timemachine
  :bind ("C-c v t" . git-timemachine))

(provide 'init-vc)
;;; init-vc.el ends here
