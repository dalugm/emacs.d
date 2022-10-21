;;; init-complete.el --- auto-completion in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Auto-completion configuration.
;;

;;; Code:

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  ;; (corfu-separator ?\s)          ; orderless field separator
  ;; (corfu-quit-at-boundary nil)   ; never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ; never quit, even if there is no match
  ;; (corfu-preview-current nil)    ; disable current candidate preview
  ;; (corfu-preselect-first nil)    ; disable candidate preselection
  ;; (corfu-on-exact-match nil)     ; configure handling of exact matches
  ;; (corfu-echo-documentation nil) ; disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ; use scroll margin
  :config
  ;; use Dabbrev with Corfu!
  (use-package dabbrev
    ;; Swap M-/ and C-M-/
    :bind (("M-/" . dabbrev-completion)
           ("C-M-/" . dabbrev-expand))
    ;; other useful Dabbrev configurations.
    :custom
    (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))))

(use-package corfu-doc
  :hook (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
              ("M-n" . corfu-doc-scroll-up)
              ("M-p" . corfu-doc-scroll-down)
              ("M-d" . corfu-doc-toggle)))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :hook (corfu-mode . corfu-terminal-mode))

(use-package corfu-doc-terminal
  :unless (display-graphic-p)
  :hook (corfu-doc-mode . corfu-doc-terminal-mode))

(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;; (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; Bind dedicated completion commands
  :bind (("C-c k p" . completion-at-point)
         ("C-c k t" . complete-tag)
         ("C-c k d" . cape-dabbrev)
         ("C-c k f" . cape-file)
         ("C-c k k" . cape-keyword)
         ("C-c k s" . cape-symbol)
         ("C-c k a" . cape-abbrev)
         ("C-c k i" . cape-ispell)
         ("C-c k l" . cape-line)
         ("C-c k w" . cape-dict)
         ("C-c k \\" . cape-tex)
         ("C-c k _" . cape-tex)
         ("C-c k ^" . cape-tex)
         ("C-c k &" . cape-sgml)
         ("C-c k r" . cape-rfc1345)))

(provide 'init-complete)

;;; init-complete.el ends here
