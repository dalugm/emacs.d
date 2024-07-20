;;; init-corfu.el --- auto complete by corfu -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Auto complete by corfu.
;;

;;; Code:

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  ;; (corfu-separator ?\s)          ; orderless field separator
  ;; (corfu-quit-at-boundary nil)   ; never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ; never quit, even if there is no match
  ;; (corfu-preview-current nil)    ; disable current candidate preview
  ;; (corfu-preselect 'prompt)      ; disable candidate preselection
  ;; (corfu-on-exact-match nil)     ; configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ; use scroll margin
  :hook
  (after-init . global-corfu-mode))

(if (display-graphic-p)
    (use-package corfu-popupinfo
      :hook (corfu-mode . corfu-popupinfo-mode)
      :bind (:map corfu-map
                  ("M-n" . corfu-popupinfo-scroll-up)
                  ("M-p" . corfu-popupinfo-scroll-down)
                  ("M-a" . corfu-popupinfo-beginning)
                  ("M-e" . corfu-popupinfo-end)
                  ("M-l" . corfu-popupinfo-location)
                  ("M-d" . corfu-popupinfo-documentation)
                  ("M-t" . corfu-popupinfo-toggle)))
  (use-package corfu-info
    :after corfu
    :bind (:map corfu-map
                ("M-l" . corfu-info-location)
                ("M-d" . corfu-info-documentation))))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :hook (corfu-mode . corfu-terminal-mode))

;; Use Dabbrev with Corfu.
(use-package dabbrev
    ;; Swap M-/ and C-M-/.
    :bind (("M-/" . dabbrev-completion)
           ("C-M-/" . dabbrev-expand))
    :config
    (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
    ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
    (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
    (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
    (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates.
  (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an
  ;; alternative, try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to
  ;; the current mode.  Corfu commands are hidden, since they are not
  ;; used via M-x. This setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic
  ;; key.  Press C-c k ? to for help.
  :bind ("C-c k" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c k d" . cape-dabbrev)
  ;;        ("C-c k h" . cape-history)
  ;;        ("C-c k f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of
  ;; `completion-at-point-functions' which is used by
  ;; `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of
  ;; buffer-local completion functions takes precedence over the
  ;; global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...

  (defun my--cape-dict-file ()
    "Return `cape-dict-file'."
    (let ((file "/usr/share/dict/words"))
      (if my-win-p
          "~/.dict"
        (list file "~/.dict"))))

  :custom
  (cape-dict-file #'my--cape-dict-file))

(provide 'init-corfu)
;;; init-corfu.el ends here
