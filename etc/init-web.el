;;; init-web.el --- Web develop configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configuration for web develop.
;;

;;; Code:

(use-package typescript-mode
  :mode "\\.ts[x]\\'")

(use-package php-mode
  :mode "\\.inc\\'"
  :config
  ;; Disable HTML compatibility in `php-mode'.
  ;; `web-mode' has superior support for php+html.
  (setq php-mode-template-compatibility nil))

(use-package emmet-mode
  :hook (css-mode web-mode html-mode nxml-mode sgml-mode))

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
  :mode "\\.wxml\\'"
  :mode "templates/.+\\.php\\'"
  :mode "wp-content/themes/.+/.+\\.php\\'"
  :custom
  ;; `web-mode-enable-auto-*' features only enabled in graphic mode
  ;; which is related on pasting issues on terminal
  ;; https://github.com/fxbois/web-mode/issues/1175
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-quoting t))

(provide 'init-web)

;;; init-web.el ends here
