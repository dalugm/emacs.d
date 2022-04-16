;;; init-ivy.el --- ivy configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; counsel => swiper => ivy.
;;

;;; Code:

(use-package ivy
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         ("C-c s s" . counsel-grep-or-swiper)
         ("C-c s g" . counsel-git)
         ("C-c s f" . my/counsel-fzf)
         ("C-c s F" . counsel-fzf)
         ("C-c s r" . my/counsel-rg)
         ("C-c s R" . counsel-rg)
         ("C-c s j" . counsel-file-jump)
         ("C-c w v" . ivy-push-view)
         ("C-c w V" . ivy-pop-view)
         ([remap recentf-open-files] . counsel-recentf)
         ([remap switch-to-buffer] . ivy-switch-buffer))
  :config
  ;; press `M-n' to insert thing-at-point into minibuffer
  ;; `M-j' to extend the minibuffer input with the next word
  (setq ivy-use-virtual-buffers t)      ; enable bookmarks and recentf
  (setq enable-recursive-minibuffers t) ; allow commands in minibuffer
  (setq ivy-wrap t)                     ; make ivy wrap around results
  (setq ivy-count-format "%d/%d ")
  ;; alternative to `C-M-j' (`ivy-immediate-done')
  (setq ivy-use-selectable-prompt t)
  ;; recenter after exiting `swiper'
  (setq swiper-action-recenter t)

  ;; better performance on everything (especially windows)
  ;; https://github.com/abo-abo/swiper/issues/1218
  (when sys/winp
    (setq ivy-dynamic-exhibit-delay-ms 250))

  ;; ---------------------------------------------------------
  ;; customize
  ;; ---------------------------------------------------------

  (defun my/counsel-rg (&optional DIR)
    "Modify `counsel-rg' functions to search files in DIR."
    (interactive "Drg in directory: ")
    (counsel-rg nil DIR))

  (defun my/counsel-fzf (&optional DIR)
    "Modify `counsel-fzf' functions to search files in DIR."
    (interactive "Dfzf in directory: ")
    (counsel-fzf nil DIR))

  (defun my//ivy--regex-plus (str)
    "Enhance `ivy--regex-plus' with special STR start pattern.
Search camel case word starting with ‘/’.
Search Chinese starting with ‘:’ by building regex using `zh-lib'."
    (my|ensure 'zh-lib)
    (let ((len (length str)))
      (cond
        ;; do nothing
        ((<= (length str) 0))
        ;; If the first character of input in ivy is ‘:’,
        ;; remaining input is converted into Zhongwen regex.
        ;; For example, input ‘:zw’ match ‘中文’, ‘植物’ and etc.
        ((string= (substring str 0 1) ":")
          (setq str (zh-lib-build-regexp-string (substring str 1 len)
                                                nil)))
        ;; If the first character of input in ivy is ‘/’,
        ;; remaining input is converted to pattern to search camel case word
        ;; For example, input ‘/ic’ match ‘isController’ or ‘IsCollapsed’
        ((string= (substring str 0 1) "/")
          (let ((rlt "")
                (i 0)
                (subs (substring str 1 len))
                c)
            (when (> len 2)
              (setq subs (upcase subs))
              (while (< i (length subs))
                (setq c (elt subs i))
                (setq rlt (concat rlt (cond
                                        ((and (< c ?a) (> c ?z)
                                              (< c ?A) (> c ?Z))
                                          (format "%c" c))
                                        (t
                                          (concat
                                            (if (= i 0)
                                                (format "[%c%c]" (+ c 32) c)
                                              (format "%c" c))
                                            "[a-z]+")))))
                (setq i (1+ i))))
            (setq str rlt))))
      (ivy--regex-plus str)))
  (setq ivy-re-builders-alist '((t . my//ivy--regex-plus)))

  ;; highlight the selected item
  (setf (alist-get 't ivy-format-functions-alist)
        #'ivy-format-function-arrow-line)

  ;; ;; https://github.com/abo-abo/swiper/issues/2213
  ;; ;; Sort ivy candidates
  ;; (setq ivy-sort-matches-functions-alist
  ;;   '((t . nil)
  ;;     (ivy-completion-in-region . ivy--shorter-matches-first)
  ;;     (ivy-switch-buffer . ivy-sort-function-buffer)
  ;;     (counsel-find-file . ivy--shorter-matches-first)))

  ;; -------------------------------------------------------
  ;; keybindings
  ;; -------------------------------------------------------

  ;; https://oremacs.com/2015/07/23/ivy-multiaction/
  ;; press `M-o' to execute `ivy-dispatching-done'
  (ivy-set-actions
    'counsel-find-file
    '(("b" counsel-find-file-cd-bookmark-action "cd bookmark")
      ("d" delete-file "delete file")
      ("j" find-file-other-frame "other frame")
      ("r" rename-file "rename file")
      ("x" counsel-find-file-as-root "open as root")
      ("X" counsel-find-file-extern "open externally")))

  (with-eval-after-load 'desktop
    ;; prevent old minibuffer completion system being reactivated in
    ;; buffers restored via `desktop.el'
    (push (cons #'counsel-mode nil) desktop-minor-mode-table)
    (push (cons #'ivy-mode nil) desktop-minor-mode-table)))

(use-package amx
  :hook (counsel-mode . amx-mode))

(provide 'init-ivy)

;;; init-ivy.el ends here
