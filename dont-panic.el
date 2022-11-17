;;; dont-panic.el --- minimal config -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Don't panic, use this minimal configuration for troubleshooting.
;;

;;; Code:

(eval-when-compile (require 'cl-lib))

;; Load path
(push (expand-file-name "etc" user-emacs-directory) load-path)

;; test Lisp downloaded from Internet here
(setq test-elisp-dir (expand-file-name "lib/site-lisp" user-emacs-directory))
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

;; Explicitly set the preferred coding systems to avoid annoying prompt
;; from Emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Better defaults
(setq inhibit-splash-screen t)
;; Show path if names are same
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
;; Deleting files go to OS's trash folder
(setq delete-by-moving-to-trash t)
;; Forbid to make backup files
(setq make-backup-files nil)
;; Disable auto save
(setq auto-save-default nil)
;; Repeating C-SPC after popping mark pops it again
(setq set-mark-command-repeat-pop t)
;; Kill line including '\n'
(setq kill-whole-line t)

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

;; Basic modes
(recentf-mode +1)
(ignore-errors (savehist-mode +1))
(save-place-mode +1)
(show-paren-mode +1)
(delete-selection-mode +1)
(global-auto-revert-mode +1)

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode +1)

(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'minibuffer-setup-hook #'subword-mode)

;; reply y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Completion
(if (fboundp 'fido-mode)
    (progn
      (fido-mode +1)
      (when (fboundp 'fido-vertical-mode)
        (fido-vertical-mode +1))

      (defun my-fido-recentf-open ()
        "Use `completing-read' to find a recent file."
        (interactive)
        (if (find-file (completing-read "Find recent file: " recentf-list))
            (message "Opening file...")
          (message "Abort.")))
      (global-set-key (kbd "C-x C-r") #'my-fido-recentf-open))
  (progn
    (ido-mode +1)
    (ido-everywhere +1)

    (setq ido-use-virtual-buffers t)
    (setq ido-use-filename-at-point 'guess)
    (setq ido-create-new-buffer 'always)
    (setq ido-enable-flex-matching t)

    (defun my-ido-recentf-open ()
      "Use `ido-completing-read' to find a recent file."
      (interactive)
      (if (find-file (ido-completing-read "Find recent file: " recentf-list))
          (message "Opening file...")
        (message "Aborting")))
    (global-set-key (kbd "C-x C-r") #'my-ido-recentf-open)))

;; Keybindings
(global-set-key (kbd "C-c i") #'imenu)
(global-set-key (kbd "C-c r") #'rectangle-mark-mode)

(defun my-eval-print-last-sexp (&optional arg)
  "Evaluate sexp before point, insert output below following an arrow.
With a `\\[universal-argument]' prefix argument ARG, delete the
sexp before point and insert output into current position."
  (interactive "P")
  (let ((value (eval (elisp--preceding-sexp))))
    (save-excursion
      (cond
       ((not arg)
        (newline-and-indent)
        (if (and (stringp value) (string-match-p "\n" value))
            ;; if return value is a multiline string
            (insert (format
                     ";; =>\n;; %S"
                     (replace-regexp-in-string "\n" "\n;; " value)))
          (insert (format "%s%S" ";; => " value))))
       ((equal arg '(4))
        (backward-kill-sexp)
        (insert (format "%S" value)))))))

(dolist (map (list emacs-lisp-mode-map
                   lisp-interaction-mode-map))
  (define-key map (kbd "C-c C-p") #'my-eval-print-last-sexp))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-x") #'ielm)
            (local-set-key (kbd "C-c C-c") #'eval-defun)
            (local-set-key (kbd "C-c C-b") #'eval-buffer)))

;; ==== put your code below this line!
