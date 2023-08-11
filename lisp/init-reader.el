;;; init-reader.el --- init for readers -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Pdf, epub, rss, etc readers.
;;

;;; Code:

(use-package elfeed
  :defer t
  :custom (url-queue-timeout 30))

(use-package elfeed-org
  :after elfeed
  :custom
  (rmh-elfeed-org-files (list
                         (expand-file-name "elfeed.org" my-optional-d)))
  :config (elfeed-org))

(use-package pdf-tools
  :when (display-graphic-p)
  :hook ((pdf-view-mode . pdf-isearch-minor-mode))
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode))

(provide 'init-reader)

;;; init-reader.el ends here
