;;; ~/.emacs.d/etc/init-evil.el --- evil configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Evil and evil-related packages configuration.
;;

;;; Code:

(use-package evil
  :init
  ;; Use Emacs keys in INSERT state.
  (setq evil-disable-insert-state-bindings t)
  :hook (after-init . evil-mode)
  :bind ((:map evil-normal-state-map
               ("]b" . next-buffer)
               ("[b" . previous-buffer)
               ("g1" . avy-goto-char-timer)
               ("g2" . avy-goto-char-2)
               ("g3" . avy-goto-word-or-subword-1)
               ("gll" . avy-goto-line)
               ("glj" . avy-goto-line-below)
               ("glk" . avy-goto-line-above)
               ("gle" . avy-goto-end-of-line)
               ("M-." . xref-find-definitions))
         (:map evil-visual-state-map
               ("v" . expreg-expand)))
  :config
  ;; Make evil-search behave more like VIM.
  (evil-select-search-module 'evil-search-module 'evil-search)
  (setq evil-ex-interactive-search-highlight 'selected-window)

  ;; https://github.com/emacs-evil/evil/issues/342
  ;; Cursor is always black because of evil.
  ;; Here is the workaround.
  (setq evil-default-cursor t)

  ;; Move back the cursor one position when exiting insert mode.
  (setq evil-move-cursor-back t)
  ;; Make cursor move as Emacs.
  (setq evil-move-beyond-eol t)
  ;; Make evil search like vim.
  (setq evil-ex-search-vim-style-regexp t)

  (defun evil-unimpaired-insert-newline-above (count)
    "Insert COUNT blank line(s) above current line."
    (interactive "p")
    (save-excursion (dotimes (_ count) (evil-insert-newline-above)))
    (when (bolp) (forward-char count)))

  (keymap-set evil-normal-state-map "[ SPC" #'evil-unimpaired-insert-newline-above)

  (defun evil-unimpaired-insert-newline-below (count)
    "Insert COUNT blank line(s) below current line."
    (interactive "p")
    (save-excursion (dotimes (_ count) (evil-insert-newline-below))))

  (keymap-set evil-normal-state-map "] SPC" #'evil-unimpaired-insert-newline-below)

  (defun my--evil-disable-ex-highlight ()
    "Disable evil ex search buffer highlight."
    (when (evil-ex-hl-active-p 'evil-ex-search)
      (evil-ex-nohighlight) t))

  (advice-add 'keyboard-quit :before #'my--evil-disable-ex-highlight)

  ;; http://blog.binchen.org/posts/code-faster-by-extending-emacs-evil-text-object
  (defun my--evil-paren-range (count beg end type inclusive)
    "Get minimum range of paren text object.
COUNT, BEG, END, TYPE is used to identify the text object.
If INCLUSIVE is t, the text object is inclusive."
    (let ((parens
           '("()" "[]" "{}" "<>" "\"\"" "''" "``"
             "（）" "《》" "「」" "『』" "【】" "〖〗"
             "“”" "‘’" "［］" "〔〕" "｛｝"))
          (pos (point))
          range
          found-range)
      (dolist (paren parens)
        (condition-case _
            (let ((char1 (aref paren 0))
                  (char2 (aref paren 1)))
              (setq range (if (eq char1 char2)
                              (evil-select-quote char1
                                                 beg end
                                                 type count inclusive)
                            (evil-select-paren char1 char2
                                               beg end
                                               type count inclusive))))
          (error nil))
        (when (and range (<= (nth 0 range) pos) (< pos (nth 1 range)))
          (cond
           (found-range
            (when (< (- (nth 1 range) (nth 0 range))
                     (- (nth 1 found-range) (nth 0 found-range)))
              (setf (nth 0 found-range) (nth 0 range))
              (setf (nth 1 found-range) (nth 1 range))))
           (t
            (setq found-range range)))))
      found-range))

  (evil-define-text-object my--evil-a-paren (count &optional beg end type)
    "Select a text object."
    :extend-selection t
    (my--evil-paren-range count beg end type t))

  (evil-define-text-object my--evil-inner-paren (count &optional beg end type)
    "Select inner text object."
    :extend-selection nil
    (my--evil-paren-range count beg end type nil))

  (keymap-set evil-outer-text-objects-map "a" #'my--evil-a-paren)
  (keymap-set evil-inner-text-objects-map "a" #'my--evil-inner-paren)

  (defun my--show-current-evil-state ()
    "Change modeline's face according to different evil state."
    (let ((color (cond
                  ((minibufferp)          my-default-color)
                  ((evil-emacs-state-p)   '("#7e1671" . "#f8f4ed"))
                  ((evil-insert-state-p)  '("#20894d" . "#f8f4ed"))
                  ((evil-visual-state-p)  '("#ffd111" . "#f8f4ed"))
                  ((evil-replace-state-p) '("#de1c31" . "#f8f4ed"))
                  ((buffer-modified-p)    '("#1772b4" . "#f8f4ed"))
                  (t                      my-default-color))))
      (set-face-background 'mode-line (car color))
      (set-face-foreground 'mode-line (cdr color))))

  (define-minor-mode my-modeline-evil-indicator-mode
    "Show current evil state by changing modeline's face."
    :global t
    :lighter ""
    (defconst my-default-color (cons (face-background 'mode-line)
                                     (face-foreground 'mode-line))
      "Default modeline color.")
    (if my-modeline-evil-indicator-mode
        (add-hook 'post-command-hook #'my--show-current-evil-state)
      (remove-hook 'post-command-hook #'my--show-current-evil-state)))

  ;; ---------------
  ;; evil keybinding
  ;; ---------------

  (evil-define-key 'normal org-mode-map
    "gh" #'outline-up-heading
    "gn" #'outline-next-visible-heading
    "gp" #'outline-previous-visible-heading
    "$"  #'org-end-of-line
    "^"  #'org-beginning-of-line
    "<"  #'org-promote-subtree          ; outdent
    ">"  #'org-demote-subtree           ; indent
    (kbd "TAB") #'org-cycle)

  (evil-define-key 'normal markdown-mode-map
    "gh" #'outline-up-heading
    "gn" #'outline-next-visible-heading
    "gp" #'outline-previous-visible-heading
    "<"  #'markdown-promote             ; outdent
    ">"  #'markdown-demote              ; indent
    (kbd "TAB") #'markdown-cycle)

  ;; ------------------
  ;; evil-initial-state
  ;; ------------------

  (defmacro my--evil-adjust-major-mode-keymap (mode &optional replace)
    "Use MODE\\='s keymap in evil after MODE loaded.

If MODE provides a feature REPLACE, to change the keymap use REPLACE instead.
URL `https://github.com/emacs-evil/evil/issues/511'."
    `(with-eval-after-load (quote ,(if replace replace mode))
       (evil-make-overriding-map ,(intern (concat mode "-mode-map")) 'normal)
       (add-hook (quote ,(intern (concat mode "-mode-hook")))
                 #'evil-normalize-keymaps)))

  (my--evil-adjust-major-mode-keymap "git-timemachine")

  (dolist (b '(
               ("+new-snippet+"  . emacs)
               ("\\*.*\\*"       . emacs)
               (".*MSG.*"        . emacs)
               ("\\*scratch\\*"  . normal)
               ))
    (add-to-list 'evil-buffer-regexps b))

  (dolist (hook '(cua-rectangle-mark-mode-hook))
    (add-hook hook #'evil-emacs-state))

  ;; Set evil initial state for major modes.
  (dolist (p '(
               (Info-mode                . emacs)
               (Man-mode                 . emacs)
               (apropos-mode             . emacs)
               (calendar-mode            . emacs)
               (compilation-mode         . emacs)
               (dired-mode               . emacs)
               (elfeed-search-mode       . emacs)
               (elfeed-show-mode         . emacs)
               (epa-key-list-mode        . emacs)
               (erc-mode                 . emacs)
               (eshell-mode              . emacs)
               (forge-post-mode          . emacs)
               (grep-mode                . emacs)
               (help-mode                . emacs)
               (image-mode               . emacs)
               (magit-mode               . emacs)
               (message-mode             . emacs)
               (minibuffer-inactive-mode . emacs)
               (profiler-report-mode     . emacs)
               (shell-mode               . emacs)
               (special-mode             . emacs)
               (term-mode                . emacs)
               (view-mode                . emacs)
               (vc-log-edit-mode         . emacs)
               (w3m-mode                 . emacs)
               (woman-mode               . emacs)
               (xref--xref-buffer-mode   . emacs)
               (fundamental-mode         . normal)
               (messages-buffer-mode     . normal)
               ))
    (evil-set-initial-state (car p) (cdr p))))

(use-package evil-surround
  :config (global-evil-surround-mode +1)

  (defmacro my--quoted-text-object (name key start-regex end-regex)
    "Define text objects for `evil-mode'.

URL `https://stackoverflow.com/a/22418983/4921402'."
    (let ((inner-name (make-symbol (concat "evil-inner-" name)))
          (outer-name (make-symbol (concat "evil-a-" name))))
      `(progn
         (evil-define-text-object
           ,inner-name (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex
                              beg end type count nil))
         (evil-define-text-object
           ,outer-name (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex
                              beg end type count t))
         (keymap-set evil-inner-text-objects-map ,key #',inner-name)
         (keymap-set evil-outer-text-objects-map ,key #',outer-name))))

  ;; NOTE: do NOT use text-object such as `w' `p'.
  (my--quoted-text-object "ShuMingHao" "q" "《" "》")
  (my--quoted-text-object "ShuangYinHao" "e" "“" "”")
  (my--quoted-text-object "DanYinHao" "d" "‘" "’")
  (my--quoted-text-object "ZhiJiaoYinHao" "r" "「" "」")
  (my--quoted-text-object "ZhiJiaoShuangYinHao" "f" "『" "』")
  (my--quoted-text-object "FangTouKuoHao" "t" "【" "】")
  (my--quoted-text-object "KongXinFangTouKuoHao" "g" "〖" "〗")
  (my--quoted-text-object "YuanKuoHao" "y" "（" "）")
  (my--quoted-text-object "QuanJiaoFangKuoHao" "u" "［" "］")
  (my--quoted-text-object "QuanJiaoWanKuoHao" "i" "〔" "〕")
  (my--quoted-text-object "QuanJiaoHuaKuoHao" "o" "｛" "｝")

  (dolist (char '(
                  (?Q . ("《 " . " 》")) (?q . ("《" . "》"))
                  (?E . ("“ "  . " ”" )) (?e . ("“"  . "”" ))
                  (?D . ("‘ "  . " ’" )) (?d . ("‘"  . "’" ))
                  (?R . ("「 " . " 」")) (?r . ("「" . "」"))
                  (?F . ("『 " . " 』")) (?f . ("『" . "』"))
                  (?T . ("【 " . " 】")) (?t . ("【" . "】"))
                  (?G . ("〖 " . " 〗")) (?g . ("〖" . "〗"))
                  (?Y . ("（ " . " ）")) (?y . ("（" . "）"))
                  (?U . ("［ " . " ］")) (?u . ("［" . "］"))
                  (?I . ("〔 " . " 〕")) (?i . ("〔" . "〕"))
                  (?O . ("｛ " . " ｝")) (?o . ("｛" . "｝"))
                  ))
    (setq-default evil-surround-pairs-alist
                  (cons char evil-surround-pairs-alist)))

  (add-hook 'org-mode-hook
            (lambda ()
              (push '(?b . ("*" . "*")) evil-surround-pairs-alist)
              (push '(?c . ("~" . "~")) evil-surround-pairs-alist)
              (push '(?i . ("/" . "/")) evil-surround-pairs-alist)
              (push '(?s . ("+" . "+")) evil-surround-pairs-alist)
              (push '(?u . ("_" . "_")) evil-surround-pairs-alist)
              (push '(?v . ("=" . "=")) evil-surround-pairs-alist)
              )))

(use-package evil-zh
  :config (global-evil-zh-mode +1))

(use-package evil-nerd-commenter
  :bind ((:map evil-normal-state-map
               ("gc" . evilnc-comment-operator)
               ("gp" . evilnc-copy-and-comment-operator)
               ("gy" . evilnc-yank-and-comment-operator))
         (:map evil-motion-state-map
               ("gc" . evilnc-comment-operator)
               ("gp" . evilnc-copy-and-comment-operator)
               ("gy" . evilnc-yank-and-comment-operator))))

(use-package general
  :config (general-evil-setup)

  ;; Use `,' as leader key.
  (general-create-definer my-comma-leader-def
    :prefix ","
    :states '(normal visual))

  (my-comma-leader-def
    ","  #'execute-extended-command
    "."  #'evil-ex
    "aa" #'avy-goto-char-2
    "ac" #'avy-goto-char-timer
    "ag" #'avy-goto-line
    "ae" #'avy-goto-end-of-line
    "aj" #'avy-goto-line-below
    "ak" #'avy-goto-line-above
    "af" #'beginning-of-defun
    "ar" #'align-regexp
    "as" #'ace-swap-window
    "aw" #'avy-goto-word-or-subword-1
    "bb" (lambda ()
           (interactive) (switch-to-buffer nil))
    "cc" #'evilnc-comment-or-uncomment-lines
    "cd" #'evilnc-copy-and-comment-lines
    "cl" #'evilnc-quick-comment-or-uncomment-to-the-line
    "cp" #'evilnc-comment-or-uncomment-paragraphs
    "cr" #'comment-or-uncomment-region
    "si" #'imenu
    "dd" #'pwd
    "dj" #'dired-jump
    "eb" #'eval-buffer
    "ee" #'eval-expression
    "el" #'eval-last-sexp
    "kb" #'kill-buffer-and-window
    "sc" #'shell-command
    "tD" #'darkroom-mode
    "td" #'darkroom-tentative-mode
    "xb" #'switch-to-buffer
    "xh" #'mark-whole-buffer
    "xf" #'find-file
    "xk" #'kill-buffer
    "xs" #'save-buffer
    ;; org
    ;; Toggle overview.
    "c$" #'org-archive-subtree
    ;; org-do-demote/org-do-premote support selected region.
    "c<"  #'org-do-promote
    "c>"  #'org-do-demote
    "cam" #'org-tags-view
    "cxi" #'org-clock-in
    "cxo" #'org-clock-out
    "cxr" #'org-clock-report
    "oa"  #'org-agenda
    "ob"  #'org-switchb
    "oc"  #'org-capture
    "ol"  #'org-store-link
    "ot"  #'org-toggle-link-display
    ;; window
    "0"   #'winum-select-window-0-or-10
    "1"   #'winum-select-window-1
    "2"   #'winum-select-window-2
    "3"   #'winum-select-window-3
    "4"   #'winum-select-window-4
    "5"   #'winum-select-window-5
    "6"   #'winum-select-window-6
    "7"   #'winum-select-window-7
    "8"   #'winum-select-window-8
    "9"   #'winum-select-window-9
    "ff"  #'my-toggle-full-window
    "oo"  #'delete-other-windows
    "sa"  #'split-window-vertically
    "sd"  #'split-window-horizontally
    "sh"  #'split-window-below
    "sq"  #'delete-window
    "sv"  #'split-window-right
    "xr"  #'my-rotate-windows
    "xt"  #'my-toggle-two-split-window
    "xo"  #'ace-window
    "x0"  #'delete-window
    "x1"  #'delete-other-windows
    "x2"  #'split-window-vertically
    "x3"  #'split-window-horizontally
    "x50" #'delete-frame
    "x51" #'delete-other-frames
    "x52" #'make-frame-command
    "x5o" #'other-frame
    ;; checker on the fly
    "fa" #'flyspell-auto-correct-word
    "fm" #'flymake-mode
    "fn" #'flyspell-goto-next-error
    "fs" #'flyspell-mode
    "ne" #'flymake-goto-next-error
    "pe" #'flymake-goto-prev-error
    ;; vc
    "va" #'vc-next-action
    "vc" #'my-vc-copy-file-and-rename-buffer
    "vf" #'my-vc-rename-file-and-buffer
    "vg" #'vc-annotate
    "vn" #'diff-hl-next-hunk
    "vp" #'diff-hl-previous-hunk
    ;; http://ergoemacs.org/emacs/emacs_pinky_2020.html
    "xx" #'my-kill-other-buffers-without-special-ones)

  ;; Use `SPC' as leader key.
  ;; All keywords arguments are still supported.
  (general-create-definer my-space-leader-def
    :prefix "SPC"
    :states '(normal visual))

  (my-space-leader-def
    "SPC" #'execute-extended-command
    ":"   #'eval-expression
    ;; bookmark/buffer
    "b"  #'(:ignore t)
    "bb" #'switch-to-buffer
    "b/" #'switch-to-buffer-other-window
    "bd" #'bookmark-delete
    "be" #'eval-buffer
    "bj" #'bookmark-jump
    "bJ" #'bookmark-jump-other-window
    "bk" #'kill-buffer
    "bl" #'bookmark-bmenu-list
    "bm" #'bookmark-set
    "bo" #'my-kill-other-buffers-without-special-ones
    "bO" #'my-kill-other-buffers-with-special-ones
    "bs" #'bookmark-save
    "bx" #'my-switch-scratch-buffer
    ;; code
    "c"  #'(:ignore t)
    "ck" #'compile
    "cr" #'recompile
    ;; dired
    "dj" #'dired-jump
    "d/" #'dired-jump-other-window
    "dd" #'pwd
    ;; file
    "f"  #'(:ignore t)
    "fb" #'my-browse-this-file
    "ff" #'find-file
    "f/" #'find-file-other-window
    "fc" #'my-copy-file-name
    "fd" #'my-delete-this-file
    "fD" #'my-delete-file
    "fo" #'my-open-file-externally
    "fr" #'my-rename-this-file
    "fs" #'my-sudo-edit-file
    "fS" #'my-sudo-find-file
    ;; git
    "g"  #'(:ignore t)
    "gd" #'magit-dispatch
    "gg" #'magit-status
    "gf" #'magit-file-dispatch
    "gs" #'consult-git-grep
    ;; load
    "l"  #'(:ignore t)
    "lF" #'my-load-font
    "lf" #'my-load-default-font
    "lt" #'load-theme
    "lg" #'ggtags-mode
    ;; org
    "o"  #'(:ignore t)
    "oa" #'org-agenda
    "ob" #'org-switchb
    "oc" #'org-capture
    "ot" #'org-toggle-link-display
    ;; search
    "s"  #'(:ignore t)
    "sd" #'search-dired-dwim
    "sD" #'search-dired
    "sf" #'my-consult-fd
    "sF" #'consult-find
    "sg" #'consult-grep
    "so" #'my-search-online
    "sr" #'consult-rg
    "ss" #'consult-line
    "si" #'imenu
    ;; toggle
    "t"  #'(:ignore t)
    "tA" #'abbrev-mode
    "tD" #'darkroom-mode
    "ta" #'auto-fill-mode
    "td" #'darkroom-tentative-mode
    "tff" #'toggle-frame-fullscreen
    "tfm" #'toggle-frame-maximized
    "tg" #'glasses-mode
    "th" #'global-hl-line-mode
    "ti" #'display-fill-column-indicator-mode
    "tj" #'toggle-truncate-lines
    "tk" #'visual-line-mode
    "tl" #'display-line-numbers-mode
    "ts" #'subword-mode
    "tv" #'view-mode
    "tw" #'whitespace-mode
    ;; window
    "w"   #'(:ignore t)
    "w'"  #'eyebrowse-last-window-config
    "w,"  #'eyebrowse-rename-window-config
    "w."  #'eyebrowse-switch-to-window-config
    "w\"" #'eyebrowse-close-window-config
    "w1"  #'eyebrowse-switch-to-window-config-1
    "w2"  #'eyebrowse-switch-to-window-config-2
    "w3"  #'eyebrowse-switch-to-window-config-3
    "w4"  #'eyebrowse-switch-to-window-config-4
    "w5"  #'eyebrowse-switch-to-window-config-5
    "w6"  #'eyebrowse-switch-to-window-config-6
    "w7"  #'eyebrowse-switch-to-window-config-7
    "w8"  #'eyebrowse-switch-to-window-config-8
    "w9"  #'eyebrowse-switch-to-window-config-9
    "wc"  #'eyebrowse-create-window-config
    "wd"  #'delete-window
    "wh"  #'evil-window-left
    "wj"  #'evil-window-down
    "wk"  #'evil-window-up
    "wl"  #'evil-window-right
    "wn"  #'eyebrowse-create-named-window-config
    "ws"  #'split-window-below
    "wv"  #'split-window-right))

(provide 'init-evil)

;;; init-evil.el ends here
