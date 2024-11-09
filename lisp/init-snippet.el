;;; init-snippet.el --- snippet for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Snippet.
;;

;;; Code:

(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete)
         ("M-_" . tempel-expand)
         ("M-*" . tempel-insert))

  :config

  (defun my--tempel-include (elt)
    "Add ELT (i template) to include templates by name in another template."
    (when (eq (car-safe elt) 'i)
      (if-let* ((template (alist-get (cadr elt) (tempel--templates))))
          (cons 'l template)
        (message "Template %s not found" (cadr elt))
        nil)))

  (add-to-list 'tempel-user-elements #'my--tempel-include)

  :init

  (defun my--tempel-setup-capf ()
    "Add the Tempel Capf to `completion-at-point-functions'.

`tempel-expand' only triggers on exact matches.  Alternatively use
`tempel-complete' if you want to see all matches, but then you should
also configure `tempel-trigger-prefix', such that Tempel does not
trigger too often when you don't expect it.  NOTE: We add
`tempel-expand' *before* the main programming mode Capf, such that it
will be tried first."
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook #'my--tempel-setup-capf)
  (add-hook 'prog-mode-hook #'my--tempel-setup-capf)
  (add-hook 'text-mode-hook #'my--tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  (global-tempel-abbrev-mode))

(provide 'init-snippet)
;;; init-snippet.el ends here
