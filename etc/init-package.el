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

(use-package embark
  :bind
  (("M-A" . embark-act)
   ("M-E" . embark-export)
   ("M-D" . embark-dwim)
   ([remap describe-bindings] . embark-bindings)
   (:map minibuffer-local-map
         ("M-a" . embark-act)
         ("M-e" . embark-export)
         ("M-d" . embark-dwim)
         ("C-c C-a" . embark-act)
         ("C-c C-o" . embark-export)
         ("C-c C-c" . embark-dwim)))
  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package zh-lib
  :custom (zh-lib-scheme 'simplified-traditional-quanpin-all))

(provide 'init-package)

;;; init-package.el ends here
