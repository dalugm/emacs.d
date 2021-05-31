;;; ~/.emacs.d/etc/init-evil.el --- evil configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Evil and evil-related packages configuration.
;;

;;; Code:

(use-package evil
  :hook (after-init . evil-mode)
  :bind (:map evil-normal-state-map
          ("]b" . next-buffer)
          ("[b" . previous-buffer)
          ("g1" . avy-goto-char-timer)
          ("g2" . avy-goto-char-2)
          ("g3" . avy-goto-word-or-subword-1)
          ("gll" . avy-goto-line)
          ("glj" . avy-goto-line-below)
          ("glk" . avy-goto-line-above)
          ("gle" . avy-goto-end-of-line)
          ("M-." . xref-find-definitions)
          ("C-]" . counsel-etags-find-tag-at-point)
         :map evil-insert-state-map
          ("C-n" . next-line)
          ("C-p" . previous-line)
          ("C-a" . beginning-of-line)
          ("C-e" . end-of-line)
          ("C-k" . kill-line)
          ("C-t" . transpose-chars)
         :map evil-visual-state-map
          ("v" . er/expand-region))
  :config
  ;; make evil-search behave more like VIM
  (evil-select-search-module 'evil-search-module 'evil-search)
  (setq evil-ex-interactive-search-highlight 'selected-window)

  ;; https://github.com/emacs-evil/evil/issues/342
  ;; Cursor is always black because of evil.
  ;; here is the workaround
  (setq evil-default-cursor t)

  ;; move back the cursor one position when exiting insert mode
  (setq evil-move-cursor-back t)
  ;; make cursor move as Emacs
  (setq evil-move-beyond-eol t)

  ;; ---------------------------------------------------------
  ;; evil enhance
  ;; ---------------------------------------------------------

  (defun evil-unimpaired-insert-newline-above (count)
    "Insert COUNT blank line(s) above current line."
    (interactive "p")
    (save-excursion (dotimes (_ count) (evil-insert-newline-above)))
    (when (bolp) (forward-char count)))

  (define-key evil-normal-state-map (kbd "[ SPC") #'evil-unimpaired-insert-newline-above)

  (defun evil-unimpaired-insert-newline-below (count)
    "Insert COUNT blank line(s) below current line."
    (interactive "p")
    (save-excursion (dotimes (_ count) (evil-insert-newline-below))))

  (define-key evil-normal-state-map (kbd "] SPC") #'evil-unimpaired-insert-newline-below)

  (defun my//evil-disable-ex-highlight ()
    "Disable evil ex search buffer highlight."
    (when (evil-ex-hl-active-p 'evil-ex-search)
      (evil-ex-nohighlight) t))

  (advice-add #'keyboard-quit :before #'my//evil-disable-ex-highlight)

  (defun my//show-current-evil-state ()
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
        (add-hook 'post-command-hook #'my//show-current-evil-state)
      (remove-hook 'post-command-hook #'my//show-current-evil-state)))

  ;; ---------------------------------------------------------
  ;; evil keybinding
  ;; ---------------------------------------------------------

  (evil-declare-key 'normal org-mode-map
    "gh" #'outline-up-heading
    "gn" #'outline-next-visible-heading
    "gp" #'outline-previous-visible-heading
    "$"  #'org-end-of-line
    "^"  #'org-beginning-of-line
    "<"  (lambda () (interactive) (my/org-demote-or-promote 1)) ; outdent
    ">"  #'my/org-demote-or-promote                             ; indent
    (kbd "TAB") #'org-cycle)

  (evil-declare-key 'normal markdown-mode-map
    "gh" #'outline-up-heading
    "gn" #'outline-next-visible-heading
    "gp" #'outline-previous-visible-heading
    "<"  (lambda () (interactive) (my/markdown-demote-or-promote 1)) ; outdent
    ">"  #'my/markdown-demote-or-promote                             ; indent
    (kbd "TAB") #'markdown-cycle)

  ;; ---------------------------------------------------------
  ;; evil-initial-state
  ;; ---------------------------------------------------------

  ;; buffer-regexps
  (dolist (b '(
                ("+new-snippet+"  . emacs)
                ("\\*.*\\*"       . emacs)
                (".*MSG.*"        . emacs)
                ("\\*scratch\\*"  . normal)
                ))
    (add-to-list 'evil-buffer-regexps b))

  ;; hook
  (dolist (hook '(org-capture-mode-hook cua-rectangle-mark-mode-hook))
    (add-hook hook #'evil-emacs-state))

  ;; specify MAJOR mode uses Evil (vim) NORMAL state or EMACS original state.
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
                (fundamental-mode         . normal)
                (forge-post-mode          . emacs)
                (grep-mode                . emacs)
                (help-mode                . emacs)
                (ivy-occur-grep-mode      . emacs)
                (ivy-occur-mode           . emacs)
                (magit-mode               . emacs)
                (message-mode             . emacs)
                (messages-buffer-mode     . normal)
                (minibuffer-inactive-mode . emacs)
                (profiler-report-mode     . emacs)
                (shell-mode               . emacs)
                (special-mode             . emacs)
                (sr-mode                  . emacs)
                (term-mode                . emacs)
                (vc-log-edit-mode         . emacs)
                (w3m-mode                 . emacs)
                (woman-mode               . emacs)
                (xref--xref-buffer-mode   . emacs)
                ))
    (evil-set-initial-state (car p) (cdr p))))

(use-package evil-surround
  :config (global-evil-surround-mode)

  (defmacro my|quoted-text-object (name key start-regex end-regex)
    "Define text objects.
ref: https://stackoverflow.com/a/22418983/4921402."
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
         (define-key evil-inner-text-objects-map ,key #',inner-name)
         (define-key evil-outer-text-objects-map ,key #',outer-name))))

  ;; NOTE: do NOT use text-object such as `w' `p'
  (my|quoted-text-object "ShuMingHao" "q" "《" "》")
  (my|quoted-text-object "ShuangYinHao" "e" "“" "”")
  (my|quoted-text-object "DanYinHao" "d" "‘" "’")
  (my|quoted-text-object "ZhiJiaoYinHao" "r" "「" "」")
  (my|quoted-text-object "ZhiJiaoShuangYinHao" "f" "『" "』")
  (my|quoted-text-object "FangTouKuoHao" "t" "【" "】")
  (my|quoted-text-object "KongXinFangTouKuoHao" "g" "〖" "〗")
  (my|quoted-text-object "YuanKuoHao" "y" "（" "）")
  (my|quoted-text-object "QuanJiaoFangKuoHao" "u" "［" "］")
  (my|quoted-text-object "QuanJiaoWanKuoHao" "i" "〔" "〕")
  (my|quoted-text-object "QuanJiaoHuaKuoHao" "o" "｛" "｝")

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

(use-package evil-matchit
  :config (global-evil-matchit-mode))

(use-package evil-zh
  :config (global-evil-zh-mode))

;; bundle with `evil'
(use-package evil-nerd-commenter
  :bind (:map evil-normal-state-map
          ("gc" . evilnc-comment-operator)
          ("gp" . evilnc-copy-and-comment-operator)
          ("gy" . evilnc-yank-and-comment-operator)
         :map evil-motion-state-map
          ("gc" . evilnc-comment-operator)
          ("gp" . evilnc-copy-and-comment-operator)
          ("gy" . evilnc-yank-and-comment-operator)))

;; https://medium.com/@schtoeffel/you-don-t-need-more-than-one-cursor-in-vim-2c44117d51db
;; https://macplay.github.io/posts/vim-bu-xu-yao-duo-guang-biao-bian-ji-gong-neng/
;; That's why I don't use multipleCursor in Emacs and VIM

(use-package general
  :config (general-evil-setup)

  ;; use `,' as leader key
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
           (interactive) (switch-to-buffer nil) :which-key "prev-buffer")
    "bu" #'backward-up-list
    "cc" #'evilnc-comment-or-uncomment-lines
    "cd" #'evilnc-copy-and-comment-lines
    "cl" #'evilnc-quick-comment-or-uncomment-to-the-line
    "cp" #'evilnc-comment-or-uncomment-paragraphs
    "cr" #'comment-or-uncomment-region
    "cg" #'counsel-grep
    "si" #'imenu
    "sr" #'my/counsel-rg
    "sf" #'my/counsel-fzf
    "dd" #'pwd
    "dj" #'dired-jump
    "eb" #'eval-buffer
    "ee" #'eval-expression
    "ef" #'end-of-defun
    "el" #'eval-last-sexp
    "fr" #'recentf-open-files
    "fl" #'recentf-load-list
    "gf" #'counsel-git
    "gs" #'counsel-git-grep
    "ir" #'ivy-resume
    "kb" #'kill-buffer-and-window
    "mf" #'mark-defun
    "op" #'smart-compile
    "sc" #'shell-command
    "ss" #'counsel-grep-or-swiper
    "tD" #'darkroom-mode
    "td" #'darkroom-tentative-mode
    "xb" #'ivy-switch-buffer
    "xc" #'save-buffers-kill-terminal
    "xh" #'mark-whole-buffer
    "xf" #'find-file
    "xk" #'kill-buffer
    "xs" #'save-buffer
    ;; org
    ;; toggle overview
    "c$" #'org-archive-subtree
    ;; org-do-demote/org-do-premote support selected region
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
    "oh"  (lambda ()
            (interactive)
            (counsel-org-agenda-headlines)
            :which-key "counsel-org-headlines")
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
    "ff"  #'(my/toggle-full-window :which-key "toggle-full-window")
    "oo"  #'delete-other-windows
    "sa"  #'split-window-vertically
    "sd"  #'split-window-horizontally
    "sh"  #'split-window-below
    "sq"  #'delete-window
    "sv"  #'split-window-right
    "xr"  #'(my/rotate-windows :which-key "rotate window")
    "xt"  #'(my/toggle-two-split-window :which-key "toggle window split")
    "xo"  #'ace-window
    "x0"  #'delete-window
    "x1"  #'delete-other-windows
    "x2"  #'split-window-vertically
    "x3"  #'split-window-horizontally
    "x50" #'delete-frame
    "x51" #'delete-other-frames
    "x52" #'make-frame-command
    "x5o" #'other-frame
    ;; `fly*-checker'
    "de" #'flycheck-display-error-at-point
    "fa" #'flyspell-auto-correct-word
    "fc" #'flycheck-mode
    "fn" #'flyspell-goto-next-error
    "fs" #'flyspell-mode
    "ne" #'flycheck-next-error
    "pe" #'flycheck-previous-error
    ;; workspace
    "ip" #'find-file-in-project
    "tt" #'find-file-in-current-directory
    "jj" #'find-file-in-project-at-point
    "kk" #'find-file-in-project-by-selected
    "fd" #'find-directory-in-project-by-selected
    ;; vc
    "va" #'vc-next-action
    "vc" #'my/vc-copy-file-and-rename-buffer
    "vf" #'my/vc-rename-file-and-buffer
    "vg" #'vc-annotate
    "vn" #'diff-hl-next-hunk
    "vp" #'diff-hl-previous-hunk
    ;; http://ergoemacs.org/emacs/emacs_pinky_2020.html
    ;; `keyfreq-show' proved sub-window operations happen most.
    "xx" #'my/kill-other-buffers-without-special-ones
    "zz" #'my/switch-to-shell)

  ;; Use `SPC' as leader key
  ;; all keywords arguments are still supported
  (general-create-definer my-space-leader-def
    :prefix "SPC"
    :states '(normal visual))

  (my-space-leader-def
    "SPC" #'execute-extended-command
    ":"   #'eval-expression
    ;; bookmark/buffer
    "b"  #'(:ignore t :which-key "bookmark")
    "bb" #'bookmark-jump
    "bd" #'bookmark-delete
    "be" #'eval-buffer
    "bj" #'bookmark-jump
    "bJ" #'bookmark-jump-other-window
    "bk" #'kill-buffer
    "bl" #'bookmark-bmenu-list
    "bL" #'(counsel-bookmarked-directory :which-key "list-bookmarked-dir")
    "bm" #'bookmark-set
    "bo" #'(my/kill-other-buffers-without-special-ones
             :which-key "keep-this-buffer-only")
    "bO" #'(my/kill-other-buffers-with-special-ones
             :which-key "keep-this-buffer-only")
    "bs" #'bookmark-save
    "bx" #'(my/switch-scratch-buffer :which-key "open-scratch")
    ;; code
    "c"  #'(:ignore t :which-key "code")
    "ck" #'compile
    "cc" #'smart-compile
    "cr" #'recompile
    ;; file
    "f"  #'(:ignore t :which-key "file")
    "fb" #'(my/browse-this-file :which-key "browse-this-file")
    "fc" #'(my/copy-this-file :which-key "copy-this-file")
    "fy" #'(my/copy-file-name :which-key "copy-file-name")
    "fd" #'(my/delete-this-file :which-key "delete-this-file")
    "fD" #'(my/delete-file :which-key "delete-file-under-cwd")
    "fj" #'(counsel-file-jump :which-key "file-jump")
    "fo" #'(my/open-this-file-externally :which-key "open-external")
    "fm" #'(my/move-this-file :which-key "move-this-file")
    "fr" #'(my/rename-this-file :which-key "rename-this-file")
    "fs" #'(my/sudo-edit-file :which-key "sudo-edit")
    "fS" #'(my/sudo-find-file :which-key "sudo-find")
    ;; git
    "g"  #'(:ignore t :which-key "git")
    "gd" #'magit-dispatch
    "gg" #'magit-status
    "gf" #'magit-file-dispatch
    "gc" #'counsel-git
    "gs" #'counsel-git-grep
    ;; hydra
    "h"  #'(:ignore t :which-key "hydra")
    "hE" #'(my/hydra-paredit-edit/body :which-key "paredit-edit")
    "hM" #'(my/hydra-paredit-move/body :which-key "paredit-move")
    "ht" #'(my/hydra-theme/body :which-key "theme")
    "hf" #'(my/hydra-file/body :which-key "file")
    "hi" #'(my/hydra-erc/body :which-key "irc")
    "hm" #'(my/hydra-misc/body :which-key "misc")
    "hp" #'(my/hydra-paredit/body :which-key "paredit")
    "hw" #'(my/hydra-window/body :which-key "window")
    ;; load
    "l"  #'(:ignore t :which-key "load")
    "lF" #'(my/load-font :which-key "font")
    "lf" #'(my/load-buffer-font :which-key "buffer-font")
    "lt" #'(load-theme :which-key "theme")
    "ll" #'lsp
    "lg" #'ggtags-mode
    ;; org
    "o"  #'(:ignore t :which-key "org")
    "oa" #'org-agenda
    "ob" #'org-switchb
    "oc" #'org-capture
    "ot" #'(org-toggle-link-display :which-key "toggle-link-display")
    ;; project
    "p"  #'(:ignore t :which-key "project")
    "pa" #'(find-file-in-project-at-point :which-key "ffip-at-point")
    "pc" #'(ffip-create-project-file :which-key "create-project-file")
    "pd" #'(find-file-in-current-directory :which-key "ffip-cwd")
    "pD" #'(find-file-in-current-directory-by-selected
             :which-key "ffip-cwd-by-select")
    "pf" #'(find-file-in-project :which-key "ffip")
    "pF" #'(ffip-lisp-find-file-in-project :which-key "ffip-lisp-ffip")
    "pi" #'(ffip-insert-file :which-key "ffip-insert-file")
    "pp" #'(find-file-in-project-at-point :which-key "ffip-at-point")
    "pr" #'(ffip-find-relative-path :which-key "ffip-find-relative-path")
    "ps" #'(find-file-in-project-by-selected :which-key "ffip-by-select")
    "pS" #'(find-file-with-similar-name :which-key "ffip-with-similar-name")
    "pv" #'(ffip-show-diff :which-key "ffip-show-diff")
    ;; search
    "s"  #'(:ignore t :which-key "search")
    "sd" #'(search-dired-dwim :which-key "search-file-cwd")
    "sD" #'search-dired
    "sf" #'(my/counsel-fzf :which-key "search-fzf")
    "sF" #'counsel-fzf
    "so" #'my/search-online
    "sr" #'(my/counsel-rg :which-key "search-rg")
    "sR" #'counsel-rg
    "ss" #'counsel-grep-or-swiper
    "si" #'imenu
    ;; toggle
    "t"  #'(:ignore t :which-key "toggle")
    "tD" #'darkroom-mode
    "ta" #'(abbrev-mode :which-key "abbrev")
    "td" #'darkroom-tentative-mode
    "tf" #'(display-fill-column-indicator-mode
             :which-key "fill-column-indicator")
    "th" #'(my/toggle-hl-line :which-key "hl-line")
    "tj" #'toggle-truncate-lines
    "tl" #'(my/toggle-line-number :which-key "line-number")
    "tp" #'(my/transient-transparency :which-key "transparency")
    "tv" #'(visual-line-mode :which-key "visual-line")
    "tw" #'(whitespace-mode :which-key "whitespace")
    ;; window
    "w"   #'(:ignore t :which-key "window")
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
