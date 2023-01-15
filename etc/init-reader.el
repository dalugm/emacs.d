;;; init-reader.el --- init for readers -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Pdf, epub, rss, etc readers.
;;

;;; Code:

;; RSS.
(use-package elfeed
  :bind ("C-c t e" . elfeed)
  :init
  (setq url-queue-timeout 30)
  (setq elfeed-curl-program-name (executable-find "curl"))
  :bind ((:map elfeed-search-mode-map
               ("A" . my-elfeed-show-all)
               ("B" . my-elfeed-show-blog)
               ("D" . my-elfeed-show-daily)
               ("E" . my-elfeed-show-emacs)
               ("q" . my-elfeed-save-db-and-bury)))
  :config
  ;; http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
  (defun my-elfeed-show-all ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-all"))
  (defun my-elfeed-show-blog ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-blog"))
  (defun my-elfeed-show-daily ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-daily"))
  (defun my-elfeed-show-emacs ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-emacs"))
  ;; Functions to support syncing `.elfeed' between machines, which
  ;; makes sure elfeed reads index from disk before launching.
  (defun my-elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))
  ;; Write to disk when quitting.
  (defun my-elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window)))

(use-package elfeed-org
  :after elfeed
  :config (elfeed-org)
  (setq rmh-elfeed-org-files (list (concat my-optional-d "elfeed.org"))))

(use-package pdf-view
  :when (display-graphic-p)
  :hook ((pdf-view-mode . pdf-isearch-minor-mode))
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  ;; Enable hiDPI support, but at the cost of memory!
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil))

(provide 'init-reader)

;;; init-reader.el ends here
