;;; init-vc.el --- version control -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Version control system.
;;

;;; Code:

;; ;; Solution 1: disable all vc backends
;; ;; https://stackoverflow.com/questions/5748814/how-does-one-disable-vc-git-in-emacs
;; (setq vc-handled-backends nil)

;; ;; Solution 2: if NO network mounted drive involved
;; (setq vc-handled-backends '(Git SVN Hg))
;; ;; https://www.reddit.com/r/emacs/comments/4c0mi3/the_biggest_performance_improvement_to_emacs_ive/
;; ;; open files faster but you can't check if file is version
;; ;; controlled. other VCS functionality still works.
;; (remove-hook 'find-file-hooks 'vc-find-file-hook)

;; ;; Solution 3: setup `vc-handled-backends' per project
;; (setq vc-handled-backends nil)
;; (defun my/setup-develop-environment ()
;;   "Default configuration for project under vcs."
;;   (interactive)
;;   (cond
;;     ((string-match-p (file-truename user-emacs-directory)
;;                      (file-name-directory (buffer-file-name)))
;;       (setq vc-handled-backends '(Git)))
;;     (t
;;       (setq vc-handled-backends nil))))
;; (dolist (hook '(java-mode-hook emacs-lisp-mode-hook org-mode-hook
;;                 js-mode-hook javascript-mode-hook web-mode-hook
;;                 c++-mode-hook c-mode-hook))
;;   (add-hook hook #'my-setup-develop-environment))

(use-package magit
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :config
  ;; add module section into the status buffer
  (magit-add-section-hook 'magit-status-sections-hook
                          #'magit-insert-modules
                          #'magit-insert-stashes 'append))

;; access GIT forges from `magit'
(use-package forge
  :after magit
  :config
  (setq forge-topic-list-columns
    '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
      ("Title" 50 t nil title  nil)
      ("State" 10 t nil state nil)
      ("Updated" 10 t nil updated nil))))

(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  ;; highlight on-the-fly
  (diff-hl-flydiff-mode +1)

  (setq diff-hl-margin-symbols-alist
    '((insert . "+") (delete . "-") (change . "=")
      (unknown . "?") (ignored . "!")))

  (unless (display-graphic-p)
    ;; fall back to margin since fringe is unavailable in terminal
    (diff-hl-margin-mode +1)
    ;; avoid restoring `diff-hl-margin-mode' when using `desktop.el'
    (with-eval-after-load 'desktop
      (push (cons #'diff-hl-margin-mode nil) desktop-minor-mode-table)))

  ;; integration with `magit'
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(use-package git-modes
  :mode ("/\\.dockerignore\\'" . gitignore-mode))

(use-package git-link
  :bind (("C-c v l" . git-link)
         ("C-c v c" . git-link-commit)
         ("C-c v h" . git-link-homepage)))

(provide 'init-vc)

;;; init-vc.el ends here
