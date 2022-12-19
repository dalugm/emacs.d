;;; init-utils.el --- utility -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Utility configuration.
;;

;;; Code:

(defconst my-linux-p (eq system-type 'gnu/linux)
  "Running on GNU/Linux.")

(defconst my-mac-p (eq system-type 'darwin)
  "Running on Mac system.")

(defconst my-cygwin-p (eq system-type 'cygwin)
  "Running on Cygwin system.")

(defconst my-win-p (eq system-type 'windows-nt)
  "Running on Windows system.")

(defconst my-mac-x-p (and (display-graphic-p) my-mac-p)
  "Running under X on Mac system.")

(defconst my-linux-x-p (and (display-graphic-p) my-linux-p)
  "Running under X on GNU/Linux system.")

(defconst my-root-p (string-equal "root" (getenv "USER"))
  "Root user.")

;; Env.
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Coding configuration, last has the highest priority.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html#Recognize-Coding
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-8)

;; Shutdown the startup screen.
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

(defun my--show-scratch-buffer-message ()
  "Customize `initial-scratch-message'."
  (let ((fortune-prog (executable-find "fortune")))
    (cond
     (fortune-prog
      (format
       ";; %s\n\n"
       (replace-regexp-in-string
        "\n" "\n;; "                ; comment each line
        (replace-regexp-in-string
         ;; remove trailing line break
         "\\(\n$\\|\\|\\[m *\\|\\[[0-9][0-9]m *\\)" ""
         (shell-command-to-string fortune-prog)))))
     (t
      (concat ";; Happy hacking "
              (or user-full-name "")
              "\n;; - Le vent se lève"
              "\n;; - il faut tenter de vivre\n\n")))))

(setq-default initial-scratch-message (my--show-scratch-buffer-message))

;; Nice scrolling.
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; Do NOT make backups of files, not safe.
;; https://github.com/joedicastro/dotfiles/tree/master/emacs
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq vc-make-backup-files nil)

(require 'recentf)
(setq recentf-max-saved-items 200)
;; Simplify save path.
(setq recentf-filename-handlers '(abbreviate-file-name))
(dolist (regexp '("^/\\(?:ssh\\|su\\|sudo\\)?x?:"
                  "/\\.?TAGS\\'" "/\\.?tags\\'"))
  (add-to-list 'recentf-exclude regexp))
;; Disable `recentf-cleanup' on recentf start,
;; because it can be laggy with remote files.
(setq recentf-auto-cleanup 'never)
(recentf-mode +1)

(setq-default buffers-menu-max-size 30
              fill-column 72
              case-fold-search t
              grep-highlight-matches t
              grep-scroll-output t
              mouse-yank-at-point t
              set-mark-command-repeat-pop t
              tooltip-delay 1.5

              ;; ;; New line at the end of file the POSIX standard
              ;; ;; defines a line is "a sequence of zero or more
              ;; ;; non-newline characters followed by a terminating
              ;; ;; newline", so files should end in a newline. Windows
              ;; ;; doesn't respect this (because it's Windows), but we
              ;; ;; should, since programmers' tools tend to be POSIX
              ;; ;; compliant.
              ;; ;; NOTE: This could accidentally edit others' code.
              ;; require-final-newline t

              truncate-lines nil
              truncate-partial-width-windows nil
              ediff-split-window-function #'split-window-horizontally
              ediff-window-setup-function #'ediff-setup-windows-plain
              ;; Disable the annoying bell ring.
              ring-bell-function #'ignore)

;;; Tab and Space
;; Indent with spaces.
(setq-default indent-tabs-mode nil)
;; But maintain correct appearance.
(setq-default tab-width 8)
;; Smart tab behavior - indent or complete.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)
;; TAB cycle if there are only few candidates.
(setq completion-cycle-threshold 3)

;; Reply y/n instead of yes/no.
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable narrowing commands.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Enabled change region case commands.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Enable erase-buffer command.
(put 'erase-buffer 'disabled nil)

;; Disable annoying blink.
(blink-cursor-mode -1)

;; Delete the selection with a key press.
(delete-selection-mode +1)

;; Fix Emacs performance when edit so-long files.
(global-so-long-mode +1)

;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode +1)

;; Automatically reload files was modified by external program.
(global-auto-revert-mode +1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Pairs...
(electric-pair-mode +1)

;; Show matching parentheses.
(show-paren-mode +1)
(setq show-paren-context-when-offscreen 'overlay)

;; Clean up obsolete buffers automatically.
(require 'midnight)

;; Undo (and redo) changes about the window.
(require 'winner)
(setq winner-boring-buffers
      '("*Completions*"
        "*Compile-Log*"
        "*inferior-lisp*"
        "*Apropos*"
        "*Help*"
        "*Buffer List*"
        "*Ibuffer*"))
(winner-mode +1)

;; Whitespace.
(require 'whitespace)
;; Search {zero,full}-width space also.
(setq whitespace-space-regexp "\\( +\\|　+\\|​+\\)")
;; Show zero-width space.
(add-to-list 'whitespace-display-mappings '(space-mark #x200b [?.]))

;; Meaningful names for buffers with the same name.
(require 'uniquify)
;; Rename after killing uniquified.
(setq uniquify-after-kill-buffer-p t)
;; Don't muck with special buffers.
(setq uniquify-ignore-buffers-re "^\\*")

(with-eval-after-load 'tramp
  (push (cons tramp-file-name-regexp nil) backup-directory-alist)

  ;; ;; https://github.com/syl20bnr/spacemacs/issues/1921
  ;; ;; If you tramp is hanging, you can uncomment below line.
  ;; (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  (setq tramp-chunksize 8192))

(with-eval-after-load 'eldoc
  ;; Multi-line message should not display too soon.
  (setq eldoc-idle-delay 1)
  (setq eldoc-echo-area-use-multiline-p t))

;; Don't ask before rereading the TAGS files if they have changed.
(setq tags-revert-without-query t)

;; Change the default behavior of hippie-expand.
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
;; use `hippie-expand' instead of `dabbrev'
(global-set-key [remap dabbrev-expand] #'hippie-expand)

(with-eval-after-load 'comint
  ;; Don't echo passwords when communicating with interactive programs:
  ;; Github prompt is like "Password for 'https://user@github.com/':"
  (setq comint-password-prompt-regexp
        (format "%s\\|^ *Password for .*: *$" comint-password-prompt-regexp))
  (add-hook 'comint-output-filter-functions
            #'comint-watch-for-password-prompt))

;; Security.
(setq auth-sources '("~/.authinfo.gpg"))

(with-eval-after-load 'epa
  ;; With GPG 2.1+, this forces gpg-agent to use the Emacs minibuffer to
  ;; prompt for the key passphrase.
  (setq epg-pinentry-mode 'loopback))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c f f") #'recentf-open-files)
(global-set-key (kbd "C-c f l") #'recentf-load-list)
(global-set-key (kbd "C-c l t") #'load-theme)
;; Be able to M-x without meta.
(global-set-key (kbd "C-c m x") #'execute-extended-command)
;; Zero width space.
(global-set-key (kbd "C-c 8 z") (lambda ()
                                  (interactive)
                                  (insert-char ?\u200b)))
;; Ideographic space.
(global-set-key (kbd "C-c 8 f") (lambda ()
                                  (interactive)
                                  (insert-char ?\u3000)))

;; Toggle.
(global-set-key (kbd "C-c t A") #'abbrev-mode)
(global-set-key (kbd "C-c t a") #'auto-fill-mode)
(global-set-key (kbd "C-c t f f") #'toggle-frame-fullscreen)
(global-set-key (kbd "C-c t f m") #'toggle-frame-maximized)
(global-set-key (kbd "C-c t g") #'glasses-mode)
(global-set-key (kbd "C-c t h") #'global-hl-line-mode)
(global-set-key (kbd "C-c t i") #'display-fill-column-indicator-mode)
(global-set-key (kbd "C-c t j") #'toggle-truncate-lines)
(global-set-key (kbd "C-c t k") #'visual-line-mode)
(global-set-key (kbd "C-c t l") #'display-line-numbers-mode)
(global-set-key (kbd "C-c t r") #'cua-rectangle-mark-mode)
(global-set-key (kbd "C-c t s") #'subword-mode)
(global-set-key (kbd "C-c t v") #'view-mode)
(global-set-key (kbd "C-c t w") #'whitespace-mode)

;; Abbrevs.
(setq save-abbrevs 'silently)

;; Search.
(global-set-key (kbd "C-c s d") #'find-dired)
(global-set-key (kbd "C-c s i") #'imenu)
(global-set-key (kbd "C-c s g") #'grep)

;; Isearch.
(global-set-key (kbd "C-M-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") #'isearch-backward-regexp)
;; Activate occur easily inside isearch.
(define-key isearch-mode-map (kbd "C-o") #'isearch-occur)

;; Align code in a pretty way.
;; http://ergoemacs.org/emacs/emacs_align_and_sort.html
(global-set-key (kbd "C-x \\") #'align-regexp)

;; Open header file under cursor.
(global-set-key (kbd "C-x C-o") #'ffap)

;; A complementary binding to the apropos-command (C-h a).
(define-key 'help-command "A" #'apropos)

(define-key 'help-command (kbd "C-f") #'find-function)
(define-key 'help-command (kbd "C-k") #'find-function-on-key)
(define-key 'help-command (kbd "C-v") #'find-variable)
(define-key 'help-command (kbd "C-l") #'find-library)

(define-key 'help-command (kbd "C-i") #'info-display-manual)

(global-set-key [remap just-one-space] #'cycle-spacing)
(global-set-key (kbd "C-x M-u") #'revert-buffer)
(global-set-key (kbd "C-x M-c") #'capitalize-region)

(global-set-key (kbd "M-z") #'zap-up-to-char)
(global-set-key (kbd "M-Z") #'zap-to-char)

(global-set-key (kbd "M-s M-j") #'scroll-other-window)
(global-set-key (kbd "M-s M-k") #'scroll-other-window-down)

(provide 'init-utils)

;;; init-utils.el ends here
