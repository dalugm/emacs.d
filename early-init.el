;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:
;;
;; The earliest bird.
;;

;;; Code:

(setq package-enable-at-startup nil)

;; Prevent outdated byte code files from being loaded.
(setq load-prefer-newer t)

;; Faster to disable these here (before they've been initialized).
(push '(tool-bar-lines . nil) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(menu-bar-lines . nil) default-frame-alist)

(when (eq system-type 'darwin)
  ;; Fix native compilation error.
  (setenv
   "LIBRARY_PATH"
   "/usr/local/opt/gcc/lib/gcc/13:/usr/local/opt/gcc/lib/gcc/13/gcc/x86_64-apple-darwin21/13"))

;;; early-init.el ends here
