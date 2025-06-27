;;; init.el --- Emacs start file -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Emacs start-up configuration.
;;

;;; Code:

(setopt user-init-file (or load-file-name buffer-file-name))
(setopt user-emacs-directory (file-name-directory user-init-file))

(defconst my-config-d (file-name-as-directory
                       (expand-file-name "etc" user-emacs-directory))
  "Directory of configuration files.")

(defconst my-library-d (file-name-as-directory
                        (expand-file-name "lib" user-emacs-directory))
  "Directory of packages.")

(defconst my-optional-d (file-name-as-directory
                         (expand-file-name "opt" user-emacs-directory))
  "Directory of optional files.")

(defconst my-cache-d (file-name-as-directory
                      (expand-file-name "var" user-emacs-directory))
  "Directory of dotfiles created by packages.")

(unless (file-directory-p my-cache-d) (mkdir my-cache-d))

(let ((old-file-name-handler-alist file-name-handler-alist)
      (old-gc-cons-threshold gc-cons-threshold))
  (setq file-name-handler-alist nil)
  (setq gc-cons-threshold most-positive-fixnum)
  (add-hook 'emacs-startup-hook
            (lambda ()
              "Speed up startup."
              (setq file-name-handler-alist
                    (delete-dups (append file-name-handler-alist
                                         old-file-name-handler-alist)))
              ;; If x10, half of cpu time is spent on gc when scrolling.
              (setq gc-cons-threshold (* 100 old-gc-cons-threshold)))))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(load
 (setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
 t t)

(require 'init-utils)
(require 'init-modeline)
(require 'init-funcs)
(require 'init-dired)
(require 'init-org)
(require 'init-ibuffer)

(when (display-graphic-p)
  (require 'init-gui))

(require 'init-package)

(require 'init-edit)
(require 'init-vc)

;; (require 'init-ido)
(require 'init-vertico)

(require 'init-corfu)

(require 'init-prog)
(require 'init-sexp)
(require 'init-check)

(require 'init-misc)
(require 'init-reader)
(require 'init-evil)
(require 'init-markup)
(require 'init-snippet)

;; Personal setup.
(load (expand-file-name "~/.custom.el") t)

(message "Emacs ready in %s with %d garbage collections."
         (format "%.2f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time)))
         gcs-done)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init.el ends here
