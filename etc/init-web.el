;;; init-web.el --- Web develop configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configuration for web develop.
;;

;;; Code:

;; New `less-css-mde' in Emacs 26
(unless (fboundp 'less-css-mode)
  (require 'less-css-mode))

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
          ("M-s g" . tagedit-goto-tag-content)
          ("M-s j" . tagedit-join-tags)
          ("M-s r" . tagedit-raise-tag)
          ("M-s s" . tagedit-splice-tag)
          ("M-s t" . tagedit-toggle-multiline-tag))
  :config (tagedit-add-experimental-features))

(provide 'init-web)

;;; init-web.el ends here
