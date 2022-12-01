;;; init-corfu.el --- auto complete by corfu -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Auto complete by corfu.
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
  ;; (corfu-scroll-margin 5)        ; use scroll margin
  :config
  ;; Use Dabbrev with Corfu.
  (use-package dabbrev
    ;; Swap M-/ and C-M-/.
    :bind (("M-/" . dabbrev-completion)
           ("C-M-/" . dabbrev-expand))
    :custom
    (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))))

(if (display-graphic-p)
    (use-package corfu-popupinfo
      :hook (corfu-mode . corfu-popupinfo-mode)
      :bind (:map corfu-map
                  ("M-n" . corfu-popupinfo-scroll-up)
                  ("M-p" . corfu-popupinfo-scroll-down)
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
  ;; Bind dedicated completion commands.
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

(provide 'init-corfu)

;;; init-corfu.el ends here
