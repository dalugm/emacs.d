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
  ;; (corfu-preselect 'prompt)      ; disable candidate preselection
  ;; (corfu-on-exact-match nil)     ; configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ; use scroll margin
  :config
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
    (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode)))

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

(use-package cape
  :init
  ;; Add to the global default value of
  ;; `completion-at-point-functions' which is used by
  ;; `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of
  ;; buffer-local completion functions takes precedence over the
  ;; global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  ;; Bind dedicated completion commands.
  :bind (("C-c k p" . completion-at-point) ; capf
         ("C-c k t" . complete-tag)        ; etags
         ("C-c k d" . cape-dabbrev)
         ("C-c k h" . cape-history)
         ("C-c k f" . cape-file)
         ("C-c k k" . cape-keyword)
         ("C-c k s" . cape-elisp-symbol)
         ("C-c k e" . cape-elisp-block)
         ("C-c k a" . cape-abbrev)
         ("C-c k l" . cape-line)
         ("C-c k :" . cape-emoji)
         ("C-c k w" . cape-dict)
         ("C-c k \\" . cape-tex)
         ("C-c k _" . cape-tex)
         ("C-c k ^" . cape-tex)
         ("C-c k &" . cape-sgml)
         ("C-c k r" . cape-rfc1345)))

(provide 'init-corfu)
;;; init-corfu.el ends here
