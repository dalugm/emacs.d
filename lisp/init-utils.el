;;; init-utils.el --- utility -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Utility configuration.
;;

;;; Code:

;;;; Const.

(defconst my-linux-p (eq system-type 'gnu/linux)
  "Running on GNU/Linux.")

(defconst my-mac-p (eq system-type 'darwin)
  "Running on Mac system.")

(defconst my-win-p (eq system-type 'windows-nt)
  "Running on Windows system.")

(defconst my-wsl-p (string-match-p "Microsoft" (shell-command-to-string "uname -a"))
  "Running on WSL (Windows Subsystem for Linux).")

;;;; Use-package.

;; It must be set before loading `use-package'.
(setopt use-package-enable-imenu-support t)

;;;; Utility.

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html
(prefer-coding-system 'utf-8)

;; Shutdown the startup screen.
(setopt inhibit-startup-screen t)

(defun my--initial-scratch-message ()
  "Customize `initial-scratch-message'."
  (format
   ;; Comment first line and add two new lines in the end.
   ";; %s\n\n"
   (replace-regexp-in-string
    ;; Comment each line below first line.
    "\n"
    "\n;; "
    (if-let ((fortune-prog (executable-find "fortune")))
        (replace-regexp-in-string
         ;; Remove extra escape sequences.
         (rx (or (seq ?\n eol)
                 (seq ?\C-\[ ?\[ (0+ digit) ?m)))
         ""
         (shell-command-to-string fortune-prog))
      (string-join
       '("Now, trailblazers"
         "Keep credos in mind"
         "(I won't say it twice!)"
         "One! Stop staying within the lines"
         "Two! We always align"
         "Three! Even if we don't gain the upper hand, we'll fight for right"
         "Four! Never care a rap for hindsight"
         "Five! Let us light the night"
         "Six! Even when there are wheels within wheels, go ahead!"
         "Get it pulverized")
       "\n")))))

(setopt initial-scratch-message (my--initial-scratch-message))

;; Warn when opening files bigger than 100MB.
(setopt large-file-warning-threshold (* 100 1000 1000))

;; Nice scrolling.
(setopt scroll-margin 0
        scroll-conservatively 100000
        scroll-preserve-screen-position 1)

;; Disable the annoying bell ring.
(setopt ring-bell-function #'ignore)

;; Use y/n instead of yes/no.
(setopt use-short-answers t)

;; Repeating C-SPC after popping mark pops it again.
(setopt set-mark-command-repeat-pop t)

;; Make mouse clicks more precise.
(setopt mouse-prefer-closest-glyph t)

;; Indent with spaces.
(setopt indent-tabs-mode nil)

;; Pass `C-u' to `recenter' to put point in the window's center.
(setopt next-error-recenter '(4))

;; Do NOT make backups of files, not safe.
;; https://github.com/joedicastro/dotfiles/tree/master/emacs
(setopt auto-save-default nil
        make-backup-files nil)

;;;; Useful modes.

;; Disable annoying blink.
(blink-cursor-mode -1)

;; Delete the selection with a key press.
(delete-selection-mode +1)

;; Fix Emacs performance when edit so-long files.
(global-so-long-mode +1)

;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode +1)

;; Pairs...
(electric-pair-mode +1)

;; Show matching parentheses.
(setopt show-paren-context-when-offscreen 'overlay)
(show-paren-mode +1)

;; Clean up obsolete buffers automatically.
(require 'midnight)

;; Restore old window configurations.
(winner-mode +1)
(setopt winner-boring-buffers '("*Apropos*" "*Buffer List*"
                                "*Completions*" "*Compile-Log*"
                                "*Help*" "*Ibuffer*"
                                "*inferior-lisp*"))

;; Automatically reload files was modified by external program.
(setopt auto-revert-verbose nil)
(global-auto-revert-mode +1)

(use-package recentf
  :hook (after-init . recentf-mode)
  :bind (("C-c f f" . recentf-open-files)
         ("C-c f l" . recentf-load-list))
  :custom
  (recentf-max-saved-items 100)
  ;; It can be laggy when cleanup remote files.
  (recentf-auto-cleanup 'never)
  :config
  (dolist (regexp '("^/\\(?:ssh\\|su\\|sudo\\)?x?:"
                    "/\\.?TAGS\\'" "/\\.?tags\\'"))
    (add-to-list 'recentf-exclude regexp)))

(use-package whitespace
  :bind ("C-c t w" . whitespace-mode)
  :custom
  ;; Search {zero,full}-width space also.
  (whitespace-space-regexp "\\( +\\|　+\\|​+\\)")
  :config
  ;; Show zero-width space.
  (add-to-list 'whitespace-display-mappings '(space-mark #x200b [?.])))

(use-package uniquify
  :defer t
  :custom
  ;; Don't muck with special buffers.
  (uniquify-ignore-buffers-re "^\\*"))

(use-package tramp
  :defer t
  :custom (tramp-default-method "ssh"))

;;;; Commands.

;; Enable narrowing commands.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Enabled change region case commands.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Enable erase-buffer command.
(put 'erase-buffer 'disabled nil)

;;;; Keybindings.

;; Be able to M-x without meta.
(keymap-global-set "C-c m x" #'execute-extended-command)

;; Zero width space.
(keymap-global-set "C-c 8 z" (lambda ()
                               (interactive)
                               (insert-char #x200b)))
;; Ideographic space.
(keymap-global-set "C-c 8 i" (lambda ()
                               (interactive)
                               (insert-char #x3000)))

;; Toggle.
(keymap-global-set "C-c t A" #'abbrev-mode)
(keymap-global-set "C-c t a" #'auto-fill-mode)
(keymap-global-set "C-c t f f" #'toggle-frame-fullscreen)
(keymap-global-set "C-c t f m" #'toggle-frame-maximized)
(keymap-global-set "C-c t g" #'glasses-mode)
(keymap-global-set "C-c t h" #'global-hl-line-mode)
(keymap-global-set "C-c t i" #'display-fill-column-indicator-mode)
(keymap-global-set "C-c t j" #'toggle-truncate-lines)
(keymap-global-set "C-c t k" #'visual-line-mode)
(keymap-global-set "C-c t l" #'display-line-numbers-mode)
(keymap-global-set "C-c t r" #'cua-rectangle-mark-mode)
(keymap-global-set "C-c t s" #'subword-mode)
(keymap-global-set "C-c t t" #'load-theme)
(keymap-global-set "C-c t v" #'view-mode)

;; Search.
(keymap-global-set "C-c s d" #'find-dired)
(keymap-global-set "C-c s i" #'imenu)
(keymap-global-set "C-c s g" #'grep)

;; Isearch.
(keymap-global-set "C-M-s" #'isearch-forward-regexp)
(keymap-global-set "C-M-r" #'isearch-backward-regexp)
;; Activate occur easily inside isearch.
(keymap-set isearch-mode-map "C-o" #'isearch-occur)

;; Align code in a pretty way.
;; http://ergoemacs.org/emacs/emacs_align_and_sort.html
(keymap-global-set "C-x \\" #'align-regexp)

;; Open header file under cursor.
(keymap-global-set "C-x C-o" #'ffap)

;; A complementary binding to the apropos-command (C-h a).
(keymap-set 'help-command "A" #'apropos)

(keymap-set 'help-command "C-f" #'find-function)
(keymap-set 'help-command "C-k" #'find-function-on-key)
(keymap-set 'help-command "C-v" #'find-variable)
(keymap-set 'help-command "C-l" #'find-library)

(keymap-set 'help-command "C-i" #'info-display-manual)

(keymap-global-set "<remap> <just-one-space>" #'cycle-spacing)
(keymap-global-set "C-x M-u" #'revert-buffer)
(keymap-global-set "C-x M-c" #'capitalize-region)

(keymap-global-set "M-z" #'zap-up-to-char)
(keymap-global-set "M-Z" #'zap-to-char)

(keymap-global-set "M-s M-j" #'scroll-other-window)
(keymap-global-set "M-s M-k" #'scroll-other-window-down)

;;;; Advice.

(define-advice delete-indentation (:around (fn &rest args) chinese)
  "Add Chinese characters support for `fixup-whitespace'.

Use `cl-letf' to change the behavior of `fixup-whitespace' only when
called from `delete-indentation'."
  (cl-letf (((symbol-function #'fixup-whitespace)
             (lambda ()
               (save-excursion
                 (delete-horizontal-space)
                 (if (or (looking-at "^\\|\\s)")
                         (save-excursion (forward-char -1)
                                         (looking-at "\\cc\\|$\\|\\s(\\|\\s'")))
                     nil
                   (insert ?\s))))))
    (apply fn args)))

(provide 'init-utils)
;;; init-utils.el ends here
