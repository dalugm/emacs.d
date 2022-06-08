;;; init.el --- Emacs start-up file -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Emacs start-up configuration.
;;

;;; Code:

(defvar my-emacs-load-start-time (current-time)
  "Record Emacs startup time.")

;;; Packages
;; Without this comment package.el adds `package-initialize' here
;; (package-initialize)

;; Avoid warnings in Emacs 27
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil))

;; HTTPS URLs should be used where possible
;; as they offer superior security
(with-eval-after-load 'package
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    (setq package-archives
      `(;; emacs-china
         ,(cons "gnu"   (concat proto "://elpa.emacs-china.org/gnu/"))
         ,(cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/"))
         ,(cons "org"   (concat proto "://elpa.emacs-china.org/org/"))
         ;; official
         ;; ,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
         ;; ,(cons "melpa" (concat proto "://melpa.org/packages/"))
         ;; ,(cons "org"   (concat proto "://orgmode.org/elpa/"))
         ))))

;; Prevents outdated byte code files from being loaded
(setq load-prefer-newer t)

;;; Define necessary directories
(defvar my-emacs-d (file-name-as-directory user-emacs-directory)
  "Directory of Emacs configurations.")

(defvar my-config-d (expand-file-name "etc/" my-emacs-d)
  "Directory of configuration files.")

(defvar my-library-d (expand-file-name "lib/" my-emacs-d)
  "Directory of packages, whether from ELPA or Github.")

(defvar my-optional-d (expand-file-name "opt/" my-emacs-d)
  "Directory of third party tools.")

(defvar my-cache-d (expand-file-name "var/" my-emacs-d)
  "Directory of dotfiles created by packages.")

(unless (file-directory-p my-cache-d) (mkdir my-cache-d))

;;; Garbage Collection
;; https://www.reddit.com/r/emacs/comments/brc05y/is_lspmode_too_slow_to_use_for_anyone_else/eofulix/
(defvar my--gc-cons-threshold-up-limit (* 100 1024 1024)
  "Best up-limit GC threshold value.  Should NOT be too big!")

(defvar my--gc-cons-threshold-default (* 20 1024 1024)
  "Default GC threshold value.")

(defun my//inc-gc-cons-threshold ()
  "Increase `gc-cons-threshold' to `my--gc-cons-threshold-up-limit'."
  (setq gc-cons-threshold my--gc-cons-threshold-up-limit))

(defun my//reset-gc-cons-threshold ()
  "Rest `gc-cons-threshold' to `my--gc-cons-threshold-default'."
  (setq gc-cons-threshold my--gc-cons-threshold-default))

;; Avoid Emacs do GC during the initializing
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(progn (my//inc-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
    (lambda ()
      (my//reset-gc-cons-threshold)
      (add-hook 'minibuffer-setup-hook #'my//inc-gc-cons-threshold)
      (add-hook 'minibuffer-exit-hook  #'my//reset-gc-cons-threshold))))

;;; Configuration
(push (expand-file-name my-config-d) load-path)

;; For speedup initialize
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; Normally value of `file-name-handler-alist' is
;;   (("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
;;   ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
;;   ("\\`/:" . file-name-non-special))
;; which means on every .el and .elc file loaded during start up, it has to run
;; those regexps against the filename.

(let ((file-name-handler-alist nil))

  (require 'init-utils)
  (require 'init-modeline)
  (require 'init-funcs)
  (require 'init-dired)
  (require 'init-org)
  (require 'init-ibuffer)
  (require 'init-gnus)
  (require 'init-package)
  (require 'init-vc)
  (require 'init-tex)
  (require 'init-vertico)
  ;; (require 'init-ivy)
  (require 'init-theme)
  (require 'init-chinese)
  (require 'init-prog)
  (require 'init-check)
  (require 'init-complete)
  (require 'init-snippet)

  ;; handy tools though not must have
  (when (display-graphic-p)
    (require 'init-gui))
  (require 'init-misc)
  (require 'init-term)
  (require 'init-workspace)
  (require 'init-reader)
  (require 'init-irc)
  ;; (require 'init-keyfreq)
  (require 'init-hydra)
  (require 'init-evil)

  ;; program
  (require 'init-sexp)
  (require 'init-lsp)
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
  (require 'init-markup)
  (require 'init-csharp)

  ;; ;; M-x `benchmark-init/show-durations-tree' to show benchmark result
  ;; (require 'benchmark-init-modes)
  ;; (require 'benchmark-init)
  ;; (benchmark-init/activate)

  ;; personal setup, other major-mode specific setup need it.
  (load (expand-file-name "~/.custom.el") t nil)

  ;; https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/
  ;; See `custom-file' for details.
  (load (setq custom-file (expand-file-name (concat my-emacs-d "custom-set-variables.el"))) t t))

(when (require 'time-date nil t)
  (message "Emacs startup time: %d seconds."
    (time-to-seconds (time-since my-emacs-load-start-time))))

;; Local Variables:
;; coding: utf-8
;; End:
;;; init.el ends here
