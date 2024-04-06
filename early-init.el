;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:
;;
;; The earliest bird.
;;

;;; Code:

(setopt package-enable-at-startup nil)

;; Prevent outdated byte code files from being loaded.
(setopt load-prefer-newer t)

;; Faster to disable these here (before they've been initialized).
(push '(tool-bar-lines . nil) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(menu-bar-lines . nil) default-frame-alist)

;;; early-init.el ends here
