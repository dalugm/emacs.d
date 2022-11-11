;;; init.el --- Emacs start-up file -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Emacs start-up configuration.
;;

;;; Code:

(setq user-init-file (or load-file-name buffer-file-name))
(setq user-emacs-directory (file-name-directory user-init-file))

(when (< emacs-major-version 27)
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(defconst my-config-d (expand-file-name "etc/" user-emacs-directory)
  "Directory of configuration files.")

(defconst my-library-d (expand-file-name "lib/" user-emacs-directory)
  "Directory of packages, whether from ELPA or Github.")

(defconst my-optional-d (expand-file-name "opt/" user-emacs-directory)
  "Directory of optional files.")

(defconst my-cache-d (expand-file-name "var/" user-emacs-directory)
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
              ;; if x10, half of cpu time is spent on gc when scrolling
              (setq gc-cons-threshold (* 100 old-gc-cons-threshold)))))

;;; Configuration
(push (expand-file-name my-config-d) load-path)

;; Load configs for specific features and modes
(require 'init-utils)
(require 'init-modeline)
(require 'init-funcs)
(require 'init-dired)
(require 'init-org)
(require 'init-ibuffer)

(require 'init-package)
(require 'init-edit)
(require 'init-vc)
;; (require 'init-ido)
(require 'init-vertico)
;; (require 'init-ivy)
(require 'init-complete)
(require 'init-prog)
(require 'init-check)
(require 'init-snippet)

;; handy tools though not must have
(when (display-graphic-p)
  (require 'init-gui))
(require 'init-misc)
(require 'init-reader)
(require 'init-irc)
(require 'init-evil)
(require 'init-markup)
(require 'init-chinese)

;; program
(require 'init-sexp)
(require 'init-lsp)
(require 'init-tex)
(require 'init-cc)
(require 'init-go)
(require 'init-js)
(require 'init-sh)
(require 'init-lua)
(require 'init-web)
(require 'init-dart)
(require 'init-rust)
(require 'init-ruby)
(require 'init-java)
(require 'init-swift)
(require 'init-build)
(require 'init-python)
(require 'init-csharp)

;; personal setup, other major-mode specific setup need it.
(load (expand-file-name "~/.custom.el") t nil)

;; https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/
;; See `custom-file' for details.
(load (setq custom-file
            (expand-file-name (concat user-emacs-directory
                                      "custom-set-variables.el")))
      t t)

(message "*** Emacs loaded in %s with %d garbage collections. ***"
         (format "%.2f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time)))
         gcs-done)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init.el ends here
