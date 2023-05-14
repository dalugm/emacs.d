;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs 27+ introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Prevents outdated byte code files from being loaded
(setq load-prefer-newer t)

;; Faster to disable these here (before they've been initialized)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; BUT there's no point in hiding the menu bar on macOS, so let's not do it
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))

(when (eq system-type 'darwin)
  ;; Fix native compilation error.
  (setenv
   "LIBRARY_PATH"
   "/usr/local/opt/gcc/lib/gcc/13:/usr/local/opt/gcc/lib/gcc/13/gcc/x86_64-apple-darwin21/13"))

;;; early-init.el ends here
