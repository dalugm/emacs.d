;;; init-web.el --- Web develop configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configuration for web develop.
;;

;;; Code:

(use-package tagedit
  :hook (html-mode . tagedit-mode)
  :bind (:map tagedit-mode-map
          ("M-k" . tagedit-kill)
          ("M-K" . tagedit-kill-attribute)
          ("M-J" . tagedit-join-tags)
          ("M-R" . tagedit-raise-tag)
          ("M-S" . tagedit-split-tag)
          ("M-?" . tagedit-convolute-tags)
          ("M-)" . tagedit-forward-slurp-tag)
          ("M-+" . tagedit-forward-barf-tag)
          ("M-s a" . te/goto-tag-begging)
          ("M-s e" . te/goto-tag-end)
          ("M-s g" . tagedit-goto-tag-content)
          ("M-s j" . tagedit-join-tags)
          ("M-s k" . te/kill-current-tag)
          ("M-s m" . te/goto-tag-match)
          ("M-s r" . tagedit-raise-tag)
          ("M-s s" . tagedit-splice-tag)
          ("M-s t" . tagedit-toggle-multiline-tag))
  :config (tagedit-add-experimental-features))

(use-package typescript-mode
  :mode "\\.ts[x]\\'")

(use-package web-mode
  :mode "\\.[px]?html?\\'"
  :mode "\\.\\(?:tpl\\|blade\\)\\(?:\\.php\\)?\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.eco\\'"
  :mode "\\.erb\\'"
  :mode "\\.hbs\\'"
  :mode "\\.jinja2?\\'"
  :mode "\\.jsp\\'"
  :mode "\\.l?eex\\'"
  :mode "\\.mustache\\'"
  :mode "\\.svelte\\'"
  :mode "\\.twig\\'"
  :mode "\\.vue\\'"
  :mode "\\.wxml\\'")

(provide 'init-web)

;;; init-web.el ends here
