;;; init-package.el --- config for manage packages -*- lexical-binding:t ; -*-

;;; Commentary:
;;
;; Package management.
;;

;;; Code:

(use-package borg
  :init
  (add-to-list 'load-path (expand-file-name "borg" my-library-d))
  :config
  (borg-initialize))

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

(use-package epkg :defer t)

(use-package epkg-marginalia
  :after (epkg marginalia)
  :config
  (setcar (alist-get 'package marginalia-annotators)
          #'epkg-marginalia-annotate-package))

(use-package no-littering
  :init
  (setopt no-littering-etc-directory my-config-d
          no-littering-var-directory my-cache-d)
  :config
  ;; Use shortened filenames.
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=56123
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-etc-directory))))

(with-eval-after-load 'package
  (setopt package-install-upgrade-built-in t)
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    (setopt package-archives
            `(

              ;; ;; Official.
              ;; ,(cons "gnu"    (concat proto "://elpa.gnu.org/packages/"))
              ;; ,(cons "nongnu" (concat proto "://elpa.nongnu.org/nongnu/"))
              ;; ,(cons "gnu-devel" (concat proto "://elpa.gnu.org/devel/"))
              ;; ,(cons "nongnu-devel" (concat proto "://elpa.nongnu.org/nongnu-devel/"))
              ;; ,(cons "melpa"  (concat proto "://melpa.org/packages/"))
              ;; ;; ,(cons "melpa-stable" (concat proto "://stable.melpa.org/packages/"))

              ;; ;; Emacs-china.
              ;; ,(cons "gnu"    (concat proto "://elpa.emacs-china.org/gnu/"))
              ;; ,(cons "nongnu" (concat proto "://elpa.emacs-china.org/nongnu/"))
              ;; ,(cons "gnu-devel" (concat proto "://elpa.emacs-china.org/gnu-devel/"))
              ;; ,(cons "nongnu-devel" (concat proto "://elpa.emacs-china.org/nongnu-devel/"))
              ;; ,(cons "melpa"  (concat proto "://elpa.emacs-china.org/melpa/"))
              ;; ;; ,(cons "melpa-stable" (concat proto "://elpa.emacs-china.org/stable-melpa/"))

              ;; ;; 163.
              ;; ,(cons "gnu"    (concat proto "://mirrors.163.com/elpa/gnu/"))
              ;; ,(cons "nongnu" (concat proto "://mirrors.163.com/elpa/nongnu/"))
              ;; ,(cons "gnu-devel" (concat proto "://mirrors.163.com/elpa/gnu-devel/"))
              ;; ,(cons "nongnu-devel" (concat proto "://mirrors.163.com/elpa/nongnu-devel/"))
              ;; ,(cons "melpa"  (concat proto "://mirrors.163.com/elpa/melpa/"))
              ;; ;; ,(cons "melpa-stable" (concat proto "://mirrors.163.com/elpa/stable-melpa/"))

              ;; Tuna.
              ,(cons "gnu"    (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
              ,(cons "nongnu" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/"))
              ,(cons "gnu-devel" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu-devel/"))
              ,(cons "nongnu-devel" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu-devel/"))
              ,(cons "melpa"  (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
              ;; ,(cons "melpa-stable" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/"))

              ))))

(provide 'init-package)
;;; init-package.el ends here
