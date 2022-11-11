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
(let ((site-lisp-dir (expand-file-name "site-lisp/" my-library-d)))
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

;; HTTPS URLs should be used where possible
;; as they offer superior security
(with-eval-after-load 'package
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    (setq package-archives
          `(

            ;; ;; official
            ;; ,(cons "gnu"    (concat proto "://elpa.gnu.org/packages/"))
            ;; ,(cons "nongnu" (concat proto "://elpa.nongnu.org/nongnu/"))
            ;; ,(cons "melpa"  (concat proto "://melpa.org/packages/"))
            ;; ;; ,(cons "melpa-stable" (concat proto "://stable.melpa.org/packages/"))
            ;; ,(cons "org"    (concat proto "://orgmode.org/elpa/"))

            ;; ;; emacs-china
            ;; ,(cons "gnu"    (concat proto "://elpa.emacs-china.org/gnu/"))
            ;; ,(cons "nongnu" (concat proto "://elpa.emacs-china.org/nongnu/"))
            ;; ,(cons "melpa"  (concat proto "://elpa.emacs-china.org/melpa/"))
            ;; ;; ,(cons "melpa-stable" (concat proto "://elpa.emacs-china.org/stable-melpa/"))
            ;; ,(cons "org"    (concat proto "://elpa.emacs-china.org/org/"))

            ;; ;; 163
            ;; ,(cons "gnu"    (concat proto "://mirrors.163.com/elpa/gnu/"))
            ;; ,(cons "nongnu" (concat proto "://mirrors.163.com/elpa/nongnu/"))
            ;; ,(cons "melpa"  (concat proto "://mirrors.163.com/elpa/melpa/"))
            ;; ;; ,(cons "melpa-stable" (concat proto "://mirrors.163.com/elpa/stable-melpa/"))
            ;; ,(cons "org"    (concat proto "://mirrors.163.com/elpa/org/"))

            ;; tuna
            ,(cons "gnu"    (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
            ,(cons "nongnu" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/"))
            ,(cons "melpa"  (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
            ;; ,(cons "melpa-stable" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/"))
            ,(cons "org"    (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/org/"))

            ))))

(provide 'init-package)

;;; init-package.el ends here
