;;; init-evil.el --- evil configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Evil and evil-related packages configuration.
;;

;;; Code:

(use-package evil
  :init
  ;; Use Emacs keys in INSERT state.
  (setq evil-disable-insert-state-bindings t)
  :hook ((after-init . evil-mode)
         (view-mode . (lambda ()
                        "Toggle `evil-state' based on current state."
                        (if (eq evil-state 'normal)
                            (evil-emacs-state)
                          (evil-normal-state)))))
  :bind (;; Evil leader key bindings.
;;;; Evil leader.
         ("<leader><SPC>" . execute-extended-command)
         ("<leader>;" . eval-expression)
         ("<leader>cc" . compile)
         ("<leader>cp" . project-compile)
         ("<leader>cr" . recompile)
;;;;; Evil leader file.
         ("<leader>ff" . find-file)
         ("<leader>fc" . my-copy-file-name)
         ("<leader>fd" . my-delete-this-file)
         ("<leader>fo" . my-open-file-externally)
         ("<leader>fr" . my-rename-this-file)
         ("<leader>f/" . find-file-other-window)
;;;;; Evil leader git.
         ("<leader>gd" . magit-dispatch)
         ("<leader>gg" . magit-status)
         ("<leader>gf" . magit-file-dispatch)
         ("<leader>gs" . consult-git-grep)
;;;;; Evil leader lsp.
         ("<leader>ll" . eglot)
         ("<leader>la" . eglot-code-actions)
         ("<leader>lc" . eglot-show-workspace-configuration)
         ("<leader>lf" . eglot-format)
         ("<leader>lq" . eglot-shutdown)
         ("<leader>lQ" . eglot-shutdown-all)
         ("<leader>lr" . eglot-rename)
;;;;; Evil leader my custom.
         ("<leader>mc" . my-avy-copy-thing-at-point)
         ("<leader>mD" . my-delete-visual-blank-lines)
         ("<leader>md" . my-delete-blank-lines)
         ("<leader>mF" . my-load-font)
         ("<leader>mf" . my-load-default-font)
         ("<leader>mp" . my-pangu-spacing-current-buffer)
         ("<leader>mT" . my-load-default-theme)
         ("<leader>mt" . my-load-theme)
         ("<leader>mx" . execute-extended-command)
;;;;; Evil leader org.
         ("<leader>oa" . org-agenda)
         ("<leader>ob" . org-switchb)
         ("<leader>oc" . org-capture)
         ("<leader>ot" . org-toggle-link-display)
;;;;; Evil leader search.
         ("<leader>sf" . consult-fd)
         ("<leader>sF" . consult-find)
         ("<leader>sg" . consult-grep)
         ("<leader>sG" . consult-git-grep)
         ("<leader>so" . my-search-online)
         ("<leader>sr" . consult-ripgrep)
         ("<leader>ss" . consult-line)
         ("<leader>su" . consult-focus-lines)
         ("<leader>sk" . consult-keep-lines)
         ("<leader>si" . imenu)
;;;;; Evil leader toggle.
         ("<leader>tA"  . abbrev-mode)
         ("<leader>ta"  . auto-fill-mode)
         ("<leader>tD"  . darkroom-mode)
         ("<leader>td"  . darkroom-tentative-mode)
         ("<leader>tff" . toggle-frame-fullscreen)
         ("<leader>tfm" . toggle-frame-maximized)
         ("<leader>tg"  . glasses-mode)
         ("<leader>th"  . global-hl-line-mode)
         ("<leader>ti"  . display-fill-column-indicator-mode)
         ("<leader>tj"  . toggle-truncate-lines)
         ("<leader>tk"  . visual-line-mode)
         ("<leader>tl"  . display-line-numbers-mode)
         ("<leader>ts"  . subword-mode)
         ("<leader>tt"  . load-theme)
         ("<leader>tv"  . view-mode)
         ("<leader>tw"  . whitespace-mode)
;;;; Evil localleader.
         ("<localleader>,"  . execute-extended-command)
         ("<localleader>."  . evil-ex)
         ("<localleader>;"  . eval-expression)
         ("<localleader>aa" . avy-goto-char-2)
         ("<localleader>ac" . avy-goto-char-timer)
         ("<localleader>ae" . avy-goto-end-of-line)
         ("<localleader>af" . beginning-of-defun)
         ("<localleader>ag" . avy-goto-line)
         ("<localleader>aj" . avy-goto-line-below)
         ("<localleader>ak" . avy-goto-line-above)
         ("<localleader>ar" . align-regexp)
         ("<localleader>aw" . avy-goto-word-or-subword-1)
         ("<localleader>bb" . (lambda () (interactive) (switch-to-buffer nil)))
         ("<localleader>cc" . evilnc-comment-or-uncomment-lines)
         ("<localleader>cd" . evilnc-copy-and-comment-lines)
         ("<localleader>cl" . evilnc-quick-comment-or-uncomment-to-the-line)
         ("<localleader>cp" . evilnc-comment-or-uncomment-paragraphs)
         ("<localleader>cr" . comment-or-uncomment-region)
         ("<localleader>dd" . pwd)
         ("<localleader>dj" . dired-jump)
         ("<localleader>dp" . project-dired)
         ("<localleader>ee" . eval-expression)
         ("<localleader>el" . eval-last-sexp)
         ("<localleader>f/" . find-file-other-window)
         ("<localleader>ff" . find-file)
         ("<localleader>gg" . switch-to-buffer)
         ("<localleader>hh" . mark-whole-buffer)
         ("<localleader>kk" . my-kill-other-buffers-without-special-ones)
         ("<localleader>ne" . flymake-goto-next-error)
         ("<localleader>pe" . flymake-goto-prev-error)
         ("<localleader>sc" . shell-command)
         ("<localleader>sf" . consult-fd)
         ("<localleader>sr" . consult-ripgrep)
         ("<localleader>ss" . consult-line)
         ("<localleader>si" . imenu)
         ("<localleader>xb" . switch-to-buffer)
         ("<localleader>xf" . find-file)
         ("<localleader>xk" . kill-buffer)
         ("<localleader>xs" . save-buffer)
         ("<localleader>xx" . kill-buffer)
         ("<localleader>vn" . diff-hl-next-hunk)
         ("<localleader>vp" . diff-hl-previous-hunk)
;;;;; Evil localleader window.
         ("<localleader>0"  . winum-select-window-0-or-10)
         ("<localleader>1"  . winum-select-window-1)
         ("<localleader>2"  . winum-select-window-2)
         ("<localleader>3"  . winum-select-window-3)
         ("<localleader>4"  . winum-select-window-4)
         ("<localleader>5"  . winum-select-window-5)
         ("<localleader>6"  . winum-select-window-6)
         ("<localleader>7"  . winum-select-window-7)
         ("<localleader>8"  . winum-select-window-8)
         ("<localleader>9"  . winum-select-window-9)
         ("<localleader>oo" . my-toggle-full-window)
         ("<localleader>sa" . split-window-vertically)
         ("<localleader>sd" . split-window-horizontally)
         ("<localleader>sh" . split-window-below)
         ("<localleader>sq" . delete-window)
         ("<localleader>sv" . split-window-right)
         ("<localleader>rr" . my-rotate-windows)
         ("<localleader>tt" . my-toggle-two-split-window)
         ("<localleader>xo" . ace-window)
         ("<localleader>ws" . ace-swap-window)
;;;; Evil state bindings.
         (:map evil-normal-state-map
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
         (:map evil-command-line-map
               ("C-a" . move-beginning-of-line)
               ("C-e" . move-end-of-line)
               ("C-f" . forward-char)
               ("C-b" . backward-char)
               ("C-d" . delete-char)
               ("C-k" . kill-line)
               ("C-o" . evil-command-window))
         (:map evil-visual-state-map
               ("V" . expreg-contract)
               ("v" . expreg-expand)))
  :custom
  (evil-ex-interactive-search-highlight 'selected-window)
  ;; Move back the cursor one position when exiting insert mode.
  (evil-move-cursor-back t)
  ;; Make cursor move as Emacs.
  (evil-move-beyond-eol t)
  ;; Make evil search like vim.
  (evil-ex-search-vim-style-regexp t)
  ;; Use native undo commands.
  (evil-undo-system 'undo-redo)
  :config
  ;; Make evil-search behave more like VIM.
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Leader key.
  (let ((state '(normal visual motion)))
    ;; SPACE as leader.
    (evil-set-leader state (kbd "SPC"))
    ;; Comma as localleader.
    (evil-set-leader state  "," t))

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

  (defun my--evil-paren-range (count beg end type inclusive)
    "Get minimum range of paren text object.

COUNT, BEG, END, TYPE is used to identify the text object.
If INCLUSIVE is t, the text object is inclusive.

URL `http://blog.binchen.org/posts/code-faster-by-extending-emacs-evil-text-object'."
    (let ((parens '("()" "[]" "{}" "<>" "\"\"" "''" "``"
                    "（）" "《》" "「」" "『』" "【】" "〖〗"
                    "“”" "‘’" "［］" "〔〕" "｛｝"))
          (pos (point))
          range found-range)
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

  (evil-define-key 'normal org-mode-map
    "gh" #'outline-up-heading
    "gn" #'outline-next-visible-heading
    "gp" #'outline-previous-visible-heading
    "$"  #'org-end-of-line
    "^"  #'org-beginning-of-line
    "<"  #'org-promote-subtree
    ">"  #'org-demote-subtree
    (kbd "TAB") #'org-cycle)

  (evil-define-key 'normal markdown-mode-map
    "gh" #'outline-up-heading
    "gn" #'outline-next-visible-heading
    "gp" #'outline-previous-visible-heading
    "<"  #'markdown-promote
    ">"  #'markdown-demote
    (kbd "TAB") #'markdown-cycle)

  (dolist (b '(;; Special buffers.
               ("\\*.*\\*"           . emacs)
               ;; Magit related.
               (".*MSG.*"            . emacs)
               ;; Yasnippet.
               ("+new-snippet+"      . emacs)
               ;; Evil-ex.
               ("\\*Command Line\\*" . normal)
               ("\\*scratch\\*"      . normal)))
    (add-to-list 'evil-buffer-regexps b))

  (setq evil-emacs-state-modes
        (append
         '(calender-mode dired-mode erc-mode image-mode)
         evil-emacs-state-modes))

  (defmacro my--evil-adjust-major-mode-keymap (mode &optional replace)
    "Use MODE\\='s keymap in `evil-normal-state' after MODE loaded.

If MODE provides a feature REPLACE, to change the keymap use REPLACE instead.
URL `https://github.com/emacs-evil/evil/issues/511'."
    `(with-eval-after-load (quote ,(if replace replace mode))
       (evil-make-overriding-map ,(intern (concat mode "-mode-map")) 'normal)
       (add-hook (quote ,(intern (concat mode "-mode-hook")))
                 #'evil-normalize-keymaps)))

  (my--evil-adjust-major-mode-keymap "git-timemachine"))

(use-package evil-zh
  :config (global-evil-zh-mode +1))

(use-package evil-surround
  :config
  (global-evil-surround-mode +1)

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

  (add-hook 'org-mode-hook
            (lambda ()
              (let ((alist '(
                             (?b . ("*" . "*"))
                             (?c . ("~" . "~"))
                             (?i . ("/" . "/"))
                             (?s . ("+" . "+"))
                             (?u . ("_" . "_"))
                             (?v . ("=" . "="))
                             )))
                (setq evil-surround-pairs-alist
                      (append alist evil-surround-pairs-alist)))))

  (let ((alist '(
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
                 )))
    (setq-default evil-surround-pairs-alist
                  (append alist evil-surround-pairs-alist))))

(use-package evil-nerd-commenter
  :bind ((:map evil-normal-state-map
               ("gc" . evilnc-comment-operator)
               ("gp" . evilnc-copy-and-comment-operator)
               ("gy" . evilnc-yank-and-comment-operator))
         (:map evil-motion-state-map
               ("gc" . evilnc-comment-operator)
               ("gp" . evilnc-copy-and-comment-operator)
               ("gy" . evilnc-yank-and-comment-operator))))

(provide 'init-evil)

;;; init-evil.el ends here
