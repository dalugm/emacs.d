;;; early-init.el --- `early-init-file' -*- lexical-binding: t -*-

;; Prevent outdated byte code files from being loaded
(setq load-prefer-newer t)

(setq package-enable-at-startup nil)

;; Tell Emacs to initialize Borg instead of Package
(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)

(require 'auto-compile)
(auto-compile-on-load-mode +1)
(auto-compile-on-save-mode +1)

;; Prevent the glimpse of un-styled Emacs
(push '(menu-bar-lines . nil) default-frame-alist)
(push '(tool-bar-lines . nil) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
