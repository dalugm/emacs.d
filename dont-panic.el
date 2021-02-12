;;; dont-panic.el --- minimal config -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Don't panic, use this minimal configuration for troubleshooting
;;

;;; Code:

(eval-when-compile (require 'cl-lib))

;; Load path
(push (expand-file-name "site-lisp" user-emacs-directory) load-path)
(push (expand-file-name "lisp" user-emacs-directory) load-path)

;; test Lisp downloaded from Internet here
(setq test-elisp-dir (expand-file-name "test" user-emacs-directory))
(unless (file-exists-p (expand-file-name test-elisp-dir))
    (make-directory (expand-file-name test-elisp-dir)))

(setq load-path
      (append
        (cl-loop for dir in (directory-files test-elisp-dir)
                 unless (string-match "^\\." dir)
                 collecting (expand-file-name (concat test-elisp-dir dir)))
        load-path))

;; Packages
;; Without this comment package.el adds (package-initialize) here
;; (package-initialize)

;; HTTPS URLs should be used where possible
;; as they offer superior security.
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

;; Explicitly set the preferred coding systems to avoid annoying prompt
;; from Emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Misc
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)   ; Deleting files go to OS's trash folder
(setq make-backup-files nil)         ; Forbid to make backup files
(setq auto-save-default nil)         ; Disable auto save
(setq set-mark-command-repeat-pop t) ; Repeating C-SPC after popping mark pops it again
(setq kill-whole-line t)             ; Kill line including '\n'

(setq-default major-mode 'text-mode)

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        8
              indent-tabs-mode nil)

;; UI
(when window-system
  (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1))
       (tool-bar-mode -1))
  (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1))
       (scroll-bar-mode -1))
  (and (fboundp 'horizontal-scroll-bar-mode)
       (horizontal-scroll-bar-mode -1)))
;; NO menu-bar
(and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1))
     (menu-bar-mode -1))

(global-hl-line-mode)

(if (fboundp 'display-line-numbers-mode)
    (global-display-line-numbers-mode)
  (global-linum-mode))

;; Basic modes
(recentf-mode)
(ignore-errors (savehist-mode))
(save-place-mode)
(show-paren-mode)
(delete-selection-mode)
(global-auto-revert-mode)

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode)

;; reply y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'minibuffer-setup-hook #'subword-mode)

;; IDO
(if (fboundp 'fido-mode)
    (progn
      (fido-mode 1)

      (defun fido-recentf-open ()
      "Use `completing-read' to find a recent file."
      (interactive)
      (if (find-file (completing-read "Find recent file: " recentf-list))
          (message "Opening file...")
        (message "Aborting")))
    (global-set-key (kbd "C-x C-r") 'fido-recentf-open))
  (progn
    (ido-mode 1)
    (ido-everywhere 1)

    (setq ido-use-virtual-buffers t)
    (setq ido-use-filename-at-point 'guess)
    (setq ido-create-new-buffer 'always)
    (setq ido-enable-flex-matching t)

    (defun ido-recentf-open ()
      "Use `ido-completing-read' to find a recent file."
      (interactive)
      (if (find-file (ido-completing-read "Find recent file: " recentf-list))
          (message "Opening file...")
        (message "Aborting")))
    (global-set-key (kbd "C-x C-r") 'ido-recentf-open)))

;; Keybindings
(global-set-key (kbd "C-.") #'imenu)
(global-set-key (kbd "<C-return>") #'rectangle-mark-mode)

(defun revert-current-buffer ()
  "Revert the current buffer."
  (interactive)
  (message "Revert this buffer")
  (text-scale-set 0)
  (widen)
  (revert-buffer t t))
(global-set-key (kbd "<f5>") #'revert-current-buffer)

(defun my/eval-last-sexp ()
  "Evaluate the last symbolic expression at the point.
With nil `C-u' prefix, insert output below following an arrow.
With one `C-u' prefix, insert output in current position.
With two `C-u' prefix, insert output in current position and delete sexp."
  (interactive)
  (let ((elisp (or (eq major-mode 'emacs-lisp-mode) (eq major-mode 'lisp-interaction-mode)))
        (clisp (eq major-mode 'common-lisp-mode))
        (scheme (eq major-mode 'scheme-mode)))
    (cond
      (elisp
        (let ((value (eval (elisp--preceding-sexp))))
          (save-excursion
            (cond
              ((equal current-prefix-arg nil) ; no prefix
                (newline-and-indent)
                (insert (format "%s%S" ";; => " value)))
              ((equal current-prefix-arg '(4)) ; one prefix
                (newline-and-indent)
                (insert (format "%S" value)))
              ((equal current-prefix-arg '(16)) ; two prefix
                (backward-kill-sexp)
                (insert (format "%S" value)))))))
      (clisp
        (message "Common Lisp mode is NOT supported yet!"))
      (scheme
        (message "Scheme mode is NOT supported yet!"))
      (t
        (message "This mode is NOT a symbolic expression related mode!")))))
(global-set-key (kbd "C-c C-e") 'my/eval-last-sexp)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-x") #'ielm)
            (local-set-key (kbd "C-c C-c") #'eval-defun)
            (local-set-key (kbd "C-c C-b") #'eval-buffer)))

;; ==== put your code below this line!

