;;; init-snippet.el --- snippet for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Snippet.
;;

;;; Code:

(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :config
  (defun my/insert-license ()
    "Insert a license template into current buffer."
    (interactive)
    (when (featurep 'evil)
      (evil-insert-state))
    (unless (gethash 'text-mode yas--tables)
      (yas-reload-all t))
    (let ((templates
           (let (yas-choose-tables-first ; avoid prompts
                 yas-choose-keys-first)
             (cl-loop for tpl in (yas--all-templates
                                  (yas--get-snippet-tables 'text-mode))
                      for uuid = (yas--template-uuid tpl)
                      if (string-prefix-p "__license-" uuid)
                      collect (cons (string-remove-prefix "__license-" uuid)
                                    tpl)))))
      (when-let (uuid (yas-choose-value (mapcar #'car templates)))
        (yas-expand-snippet (cdr (assoc uuid templates))))))

  (setq my-private-snippet (expand-file-name "~/my-snippets"))
  (when (and (file-exists-p my-private-snippet)
             (not (member my-yasnippets yas-snippet-dirs)))
    (add-to-list 'yas-snippet-dirs my-private-snippet)))

(use-package auto-yasnippet
  :after yasnippet
  :bind ((:map yas-minor-mode-map
          ("C-c e c" . aya-create)
          ("C-c e p" . aya-expand)
          ("C-c e l" . aya-open-line))))

(provide 'init-snippet)

;;; init-snippet.el ends here
