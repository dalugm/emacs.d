;;; init-package.el --- config for manage packages -*- lexical-binding:t ; -*-

;;; Commentary:
;;
;; Package management.
;;

;;; Code:

(eval-and-compile                       ; `borg'
  (add-to-list 'load-path (expand-file-name "borg" my-library-d))
  (require 'borg)
  (setq borg-drones-directory my-library-d
        borg-user-emacs-directory user-emacs-directory
        borg-gitmodules-file (expand-file-name ".gitmodules"
                                               user-emacs-directory))
  ;; use HTTPS instead of SSH
  (setq borg-rewrite-urls-alist
        '(("git@github.com:" . "https://github.com/")
          ("git@gitlab.com:" . "https://gitlab.com/")))
  (borg-initialize))

;; Add both site-lisp and its subdirs to `load-path'
(let ((site-lisp-dir (expand-file-name "lib/site-lisp/"
                                       user-emacs-directory)))
  (push site-lisp-dir load-path)
  (my--add-subdirs-to-load-path site-lisp-dir))

(eval-and-compile                       ; `use-package'
  (require 'use-package)
  (setq use-package-verbose t))

(use-package auto-compile
  :custom
  (auto-compile-display-buffer               nil)
  (auto-compile-mode-line-counter            t)
  (auto-compile-source-recreate-deletes-dest t)
  (auto-compile-toggle-deletes-nonlib-dest   t)
  (auto-compile-update-autoloads             t)
  :config
  (auto-compile-on-load-mode +1)
  (auto-compile-on-save-mode +1))

(use-package epkg
  :defer t
  :init
  (setq epkg-database-connector
        (if (>= emacs-major-version 29)
            'sqlite-builtin
          'sqlite-module)))

(use-package epkg-marginalia
  :after epkg
  :config
  (cl-pushnew 'epkg-marginalia-annotate-package
              (alist-get 'package marginalia-annotator-registry)))

(use-package no-littering
  :demand t
  :init
  (setq no-littering-etc-directory my-config-d
        no-littering-var-directory my-cache-d)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(provide 'init-package)

;;; init-package.el ends here
