;;; init-web.el --- Web develop configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configuration for web develop.
;;

;;; Code:

(use-package php-mode
  :mode "\\.inc\\'"
  :config
  ;; Disable HTML compatibility in `php-mode'.
  ;; `web-mode' has superior support for php+html.
  (setq php-mode-template-compatibility nil))

(use-package emmet-mode
  :hook (js-mode css-mode web-mode html-mode nxml-mode sgml-mode))

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
  :mode "\\.n?vue\\'"
  :mode "\\.tsx\\'"
  :mode "\\.wxml\\'"
  :mode "templates/.+\\.php\\'"
  :mode "wp-content/themes/.+/.+\\.php\\'"
  :custom
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  ;; `web-mode-enable-auto-*' features only enabled in graphic mode
  ;; which is related on pasting issues on terminal
  ;; https://github.com/fxbois/web-mode/issues/1175
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-auto-pairing nil))

(use-package pug-mode
  :mode "\\.pug\\'")

(provide 'init-web)

;;; init-web.el ends here
