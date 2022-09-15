;;; init-package.el --- config for manage packages -*- lexical-binding:t ; -*-

;;; Commentary:
;;
;; Package management.
;;

;;; Code:

(add-to-list 'load-path (expand-file-name "compat" my-library-d))
(add-to-list 'load-path (expand-file-name "packed" my-library-d))
(add-to-list 'load-path (expand-file-name "auto-compile" my-library-d))

(require 'auto-compile)
(auto-compile-on-load-mode +1)
(auto-compile-on-save-mode +1)

(progn                                  ; `borg'
  (add-to-list 'load-path (expand-file-name "borg" my-library-d))
  (require 'borg)
  (setq borg-drones-directory my-library-d
        borg-user-emacs-directory my-emacs-d
        borg-gitmodules-file (expand-file-name ".gitmodules" my-emacs-d))
  ;; use HTTPS instead of SSH
  (setq borg-rewrite-urls-alist
        '(("git@github.com:" . "https://github.com/")
          ("git@gitlab.com:" . "https://gitlab.com/")))
  (borg-initialize))

(eval-and-compile                       ; `use-package'
  (require 'use-package)
  (setq use-package-verbose t))

(use-package auto-compile
  :custom
  (auto-compile-display-buffer               nil)
  (auto-compile-mode-line-counter            t)
  (auto-compile-source-recreate-deletes-dest t)
  (auto-compile-toggle-deletes-nonlib-dest   t)
  (auto-compile-update-autoloads             t))

(use-package epkg
  :defer t
  :init
  (setq epkg-database-connector
        (if (>= emacs-major-version 29)
            'sqlite-builtin
          'sqlite-module)))

(use-package wgrep)

(use-package no-littering
  :demand t
  :init
  (setq no-littering-etc-directory my-config-d
        no-littering-var-directory my-cache-d)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;;; Useful packages
;; which should be loaded before any other packages

(use-package which-key
  :bind ("C-h M-m" . which-key-show-major-mode)
  :hook (after-init . which-key-mode)
  :custom
  (which-key-allow-imprecise-window-fit t) ; performance
  (which-key-separator ":")
  :config
  (which-key-add-key-based-replacements "C-x RET" "coding-system")
  (which-key-add-key-based-replacements "C-x 8" "unicode")
  (which-key-add-key-based-replacements "C-x @" "modifior")
  (which-key-add-key-based-replacements "C-x X" "edebug")
  (which-key-add-key-based-replacements "C-x a" "abbrev")
  (which-key-add-key-based-replacements "C-x n" "narrow")
  (which-key-add-key-based-replacements "C-x t" "tab")
  (which-key-add-key-based-replacements "C-x C-a" "edebug")

  (which-key-add-key-based-replacements "C-c !" "check")
  (which-key-add-key-based-replacements "C-c &" "yasnippet")
  (which-key-add-key-based-replacements "C-c @" "hideShow")
  (which-key-add-key-based-replacements "C-c c" "code")
  (which-key-add-key-based-replacements "C-c c q" "quickrun")
  (which-key-add-key-based-replacements "C-c e" "edit")
  (which-key-add-key-based-replacements "C-c f" "file")
  (which-key-add-key-based-replacements "C-c g" "goto")
  (which-key-add-key-based-replacements "C-c h" "hydra")
  (which-key-add-key-based-replacements "C-c l" "load")
  (which-key-add-key-based-replacements "C-c m" "misc")
  (which-key-add-key-based-replacements "C-c o" "org")
  (which-key-add-key-based-replacements "C-c p" "project")
  (which-key-add-key-based-replacements "C-c s" "search")
  (which-key-add-key-based-replacements "C-c t" "toggle")
  (which-key-add-key-based-replacements "C-c v" "vc")
  (which-key-add-key-based-replacements "C-c v l" "link")
  (which-key-add-key-based-replacements "C-c w" "workspace")

  (which-key-add-major-mode-key-based-replacements 'python-mode
    "C-c C-t" "python-skeleton")

  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-a" "markdown-link")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-c" "markdown-command")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-s" "markdown-style")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-t" "markdown-header")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-x" "markdown-toggle")

  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-a" "markdown-link")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-c" "markdown-command")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-s" "markdown-style")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-t" "markdown-header")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-x" "markdown-toggle"))

;; avy, jump between texts, like vim-easymotion
;; https://emacsredux.com/blog/2015/07/19/ace-jump-mode-is-dead-long-live-avy/ for more tips
(use-package avy
  :hook (after-init . avy-setup-default)
  :bind (("C-c g 2" . avy-goto-char-2)
         ("C-c g c" . avy-goto-char)
         ("C-c g e" . avy-goto-end-of-line)
         ("C-c g g" . avy-goto-char-timer)
         ("C-c g i" . avy-goto-char-in-line)
         ("C-c g j" . avy-goto-line-below)
         ("C-c g k" . avy-goto-line-above)
         ("C-c g l" . avy-goto-line)
         ("C-c g w" . avy-goto-word-or-subword-1))
  :custom (avy-style 'at-full))

(use-package zh-lib
  :custom (zh-lib-scheme 'simplified-traditional-quanpin-all))

(use-package smart-compile
  :bind ("C-c c c" . smart-compile))

(use-package expand-region
  :bind ("C-c ;" . er/expand-region))

(provide 'init-package)

;;; init-package.el ends here
