;;; init-utils.el --- utility -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Utility configuration.
;;

;;; Code:

(require 'init-const)

(defmacro my|ensure (feature)
  "Make sure FEATURE is required."
  `(unless (featurep ,feature)
     (condition-case nil
       (require ,feature)
       (error nil))))

(defmacro my|measure-time (&rest body)
  "Measure the time takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06fs" (float-time (time-since time)))))

(setq user-full-name "dalu")
(setq user-mail-address "moutong945@outlook.com")

;;; env
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; coding configuration, last has highest priority
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html#Recognize-Coding
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-8)

;; shutdown the startup screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

(defun my//show-scratch-buffer-message ()
  "Customize `initial-scratch-message'."
  (let ((fortune-prog (or (executable-find "fortune-zh")
                          (executable-find "fortune"))))
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

(setq-default initial-scratch-message (my//show-scratch-buffer-message))

;; nice scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; do NOT make backups of files, not safe
;; https://github.com/joedicastro/dotfiles/tree/master/emacs
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq vc-make-backup-files nil)

(setq-default buffers-menu-max-size 30
              fill-column 80
              case-fold-search t
              compilation-scroll-output t
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              grep-highlight-matches t
              grep-scroll-output t
              line-spacing 0
              mouse-yank-at-point t
              set-mark-command-repeat-pop t
              tooltip-delay 1.5

              ;; ;; new line at the end of file
              ;; ;; the POSIX standard defines a line is "a sequence of zero or more non-newline
              ;; ;; characters followed by a terminating newline", so files should end in a
              ;; ;; newline. Windows doesn't respect this (because it's Windows), but we should,
              ;; ;; since programmers' tools tend to be POSIX compliant.
              ;; ;; NOTE: This could accidentally edit others' code
              ;; require-final-newline t

              truncate-lines nil
              truncate-partial-width-windows nil
              ;; visible-bell has some issue
              ;; https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/issues/9#issuecomment-97848938
              visible-bell nil
              ;; disable the annoying bell ring
              ring-bell-function 'ignore)

;;; Tab and Space
;; indent with spaces
(setq-default indent-tabs-mode nil)
;; but maintain correct appearance
(setq-default tab-width 8)
;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; reply y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

;; disable annoying blink
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; delete the selection with a key press
(delete-selection-mode +1)

;; show matching parentheses
(show-paren-mode +1)
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;; fix Emacs performance when edit so-long files
(when (fboundp 'so-long-enable)
  (add-hook 'after-init-hook #'so-long-enable))

;; https://www.emacswiki.org/emacs/SavePlace
(cond
  ((fboundp 'save-place-mode)
    (save-place-mode +1))
  (t
    (require 'saveplace)
    (setq-default save-place t)))

(unless (or sys/cygwinp sys/winp)
  ;; Takes ages to start Emacs.
  ;; Got error `Socket /tmp/fam-cb/fam- has wrong permissions` in Cygwin ONLY!
  ;; reproduced with Emacs 26.1 and Cygwin upgraded at 2019-02-26
  ;;
  ;; Although win64 is fine. It still slows down generic performance.
  ;; https://stackoverflow.com/questions/3589535/why-reload-notification-slow-in-emacs-when-files-are-modified-externally
  ;; So no `auto-revert-mode' on Windows/Cygwin
  (global-auto-revert-mode +1)
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil))

;; clean up obsolete buffers automatically
(require 'midnight)

;; "Undo"(and "redo") changes in the window configuration with the key commands.
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

;; `whitespace-mode' config
(require 'whitespace)
(setq whitespace-line-column 80)        ; limit line length
(setq whitespace-style '(face indentation
                         tabs tab-mark
                         spaces space-mark
                         newline newline-mark
                         trailing lines-tail))
(setq whitespace-display-mappings '((tab-mark ?\t [?› ?\t])
                                    (space-mark ?\  [?·] [?.])
                                    (newline-mark ?\n [?¬ ?\n])))

;; `tramp-mode' config
(with-eval-after-load 'tramp
  (push (cons tramp-file-name-regexp nil) backup-directory-alist)

;; ;; https://github.com/syl20bnr/spacemacs/issues/1921
;; ;; If you tramp is hanging, you can uncomment below line.
;; (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  (setq tramp-chunksize 8192))

;; NOTE: `tool-bar-mode' and `scroll-bar-mode' are not defined in some cases
;; https://emacs-china.org/t/topic/5159/12
;; https://github.com/vijaykiran/spacemacs/commit/b2760f33e5c77fd4a073bc052e7b3f95eedae08f
;; removes the GUI elements
;; NO scroll-bar, tool-bar
(when window-system
  (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1))
    (tool-bar-mode -1))
  (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1))
    (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1)))

;; NO menu-bar
;; BUT there's no point in hiding the menu bar on mac, so let's not do it
(unless sys/mac-x-p
  (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1))
    (menu-bar-mode -1)))

;; recentf
(require 'recentf)
(setq recentf-max-saved-items 200)
(setq recentf-max-menu-items 15)

;; disable `recentf-cleanup' on Emacs start,
;; because it can cause problems with remote files
(setq recentf-auto-cleanup 'never)

;; Simplify save path
(setq recentf-filename-handlers '(abbreviate-file-name))

(add-to-list 'recentf-exclude '("^/\\(?:ssh\\|su\\|sudo\\)?:" "/TAGS\\'"))

(recentf-mode +1)

;; eldoc
(with-eval-after-load 'eldoc
  ;; multi-line message should not display too soon
  (setq eldoc-idle-delay 1)
  (setq eldoc-echo-area-use-multiline-p t))

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; bookmark
(require 'bookmark)
(setq bookmark-save-flag 1)

;; hippie-expand
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
(global-set-key (kbd "M-/") #'hippie-expand)

(with-eval-after-load 'comint
  ;; Don't echo passwords when communicating with interactive programs:
  ;; Github prompt is like "Password for 'https://user@github.com/':"
  (setq comint-password-prompt-regexp
    (format "%s\\|^ *Password for .*: *$" comint-password-prompt-regexp))
  (add-hook 'comint-output-filter-functions
            #'comint-watch-for-password-prompt))

;; security
(setq auth-sources '("~/.authinfo.gpg"))

(with-eval-after-load 'epa
  ;; with GPG 2.1+, this forces gpg-agent to use the Emacs minibuffer to prompt
  ;; for the key passphrase.
  ;; `epa-pinentry-mode' is obsolete since Emacs 27.1
  (set (if emacs/>=27p
           'epg-pinentry-mode
         'epa-pinentry-mode)
       'loopback))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c l t") #'load-theme)
(global-set-key (kbd "C-c f f") #'recentf-open-files)
(global-set-key (kbd "C-c f l") #'recentf-load-list)
;; be able to M-x without meta
(global-set-key (kbd "C-c m x") #'execute-extended-command)
(global-set-key (kbd "C-x 8 s") (lambda ()
                                  (interactive)
                                  (insert "　")))

;; toggle
(global-set-key (kbd "C-c t a") #'abbrev-mode)
(global-set-key (kbd "C-c t f") #'display-fill-column-indicator-mode)
(global-set-key (kbd "C-c t g") #'glasses-mode)
(global-set-key (kbd "C-c t j") #'toggle-truncate-lines)
(global-set-key (kbd "C-c t r") #'cua-rectangle-mark-mode)
(global-set-key (kbd "C-c t s") #'subword-mode)
(global-set-key (kbd "C-c t v") #'visual-line-mode)
(global-set-key (kbd "C-c t w") #'whitespace-mode)

;; abbrevs
(setq save-abbrevs 'silently)
(define-abbrev-table 'global-abbrev-table '(
                                             ;; signature
                                             ("mt" "dalu")
                                             ;; Emacs regex
                                             ("wn" "\\([A-Za-z0-9]+\\)" )
                                             ;; unicode
                                             ("fws" "　")
                                             )
  "Abbrev table for my own use.")

;;; Search
(global-set-key (kbd "C-c s d") #'find-dired)
(global-set-key (kbd "C-c s i") #'imenu)

;; isearch
(global-set-key (kbd "C-M-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") #'isearch-backward-regexp)
;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o") #'isearch-occur)

;; align code in a pretty way
;; http://ergoemacs.org/emacs/emacs_align_and_sort.html
(global-set-key (kbd "C-x \\") #'align-regexp)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ; back one

;; open header file under cursor
(global-set-key (kbd "C-x C-o") #'ffap)

;;; help-command

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" #'apropos)

(define-key 'help-command (kbd "C-f") #'find-function)
(define-key 'help-command (kbd "C-k") #'find-function-on-key)
(define-key 'help-command (kbd "C-v") #'find-variable)
(define-key 'help-command (kbd "C-l") #'find-library)

(define-key 'help-command (kbd "C-i") #'info-display-manual)

;; misc
(global-set-key [remap just-one-space] #'cycle-spacing)
(global-set-key (kbd "C-x M-u") #'revert-buffer)
(global-set-key (kbd "C-x M-c") #'capitalize-region)

(global-set-key (kbd "M-z") #'zap-up-to-char)
(global-set-key (kbd "M-Z") #'zap-to-char)

(provide 'init-utils)

;;; init-utils.el ends here
