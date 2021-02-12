;;; init-snippet.el --- snippet for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Snippet.
;;

;;; Code:

;; https://stackoverflow.com/questions/7619640/emacs-latex-yasnippet-why-are-newlines-inserted-after-a-snippet
(use-package yasnippet
  :init
  ;; Insert license
  (defun my/insert-license ()
    "Insert a license file template into the current file."
    (interactive)
    (my|ensure 'yasnippet)
    (when (featurep 'evil)
      (evil-insert-state))
    (yas-minor-mode-on)
    (unless (gethash 'text-mode yas--tables)
      (yas-reload-all t))
    (let ((templates
           (let (yas-choose-tables-first ; avoid prompts
                 yas-choose-keys-first)
             (cl-loop for tpl in (yas--all-templates (yas--get-snippet-tables 'text-mode))
                      for uuid = (yas--template-uuid tpl)
                      if (string-prefix-p "__license-" uuid)
                      collect (cons (string-remove-prefix "__license-" uuid) tpl)))))
      (when-let (uuid (yas-choose-value (mapcar #'car templates)))
        (yas-expand-snippet (cdr (assoc uuid templates))))))
  :bind (("C-c t y" . yas-minor-mode)
         ("C-c t Y" . yas-global-mode))
  :config (yas-reload-all)
  (setq my-private-snippet (expand-file-name "my-snippets/" my-optional-d))
  (when (and (file-exists-p my-private-snippet)
             (not (member my-yasnippets yas-snippet-dirs)))
    (add-to-list 'yas-snippet-dirs my-private-snippet)))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package auto-yasnippet
  :after yasnippet
  :bind (:map yas-minor-mode-map
          ("C-c t c" . aya-create)
          ("C-c t e" . aya-expand)
          ("C-c t o" . aya-open-line)))

(provide 'init-snippet)

;;; init-snippet.el ends here
