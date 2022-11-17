;;; init-edit.el --- edit in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Emacs is the best editor ever.
;;

;;; Code:

(use-package zh-lib
  :custom (zh-lib-scheme 'simplified-traditional-quanpin-all))

(use-package marginalia
  :config (marginalia-mode +1))

(use-package embark
  :bind (("M-A" . embark-act)
         ("M-E" . embark-export)
         ("M-D" . embark-dwim)
         ([remap describe-bindings] . embark-bindings)
         (:map minibuffer-local-map
               ("M-a" . embark-act)
               ("M-e" . embark-export)
               ("M-d" . embark-dwim)
               ("M-." . my-embark-preview)
               ("C-c C-a" . embark-act)
               ("C-c C-o" . embark-export)
               ("C-c C-c" . embark-dwim)))
  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  :config
  (defun my-embark-preview ()
    "Previews candidate in vertico buffer, unless it's a consult command"
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim)))))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package color-rg
  :custom
  (color-rg-recenter-match-line t)
  (color-rg-mac-load-path-from-shell nil)
  :bind
  (("C-c s s" . color-rg-search-input)
   ("C-c s c" . color-rg-search-input-in-current-file)
   ("C-c s C" . color-rg-search-symbol-in-current-file)
   ("C-c s S" . color-rg-customized-search)
   ("C-c s p" . color-rg-search-project)
   ("C-c s M-n" . color-rg-search-symbol)
   (:map isearch-mode-map
         ("M-s M-s" . isearch-toggle-color-rg))
   ;; vim-like
   (:map color-rg-mode-map
         ("h" . color-rg-jump-prev-file)
         ("l" . color-rg-jump-next-file))
   (:map color-rg-mode-edit-map
         ("C-c C-h" . color-rg-jump-prev-file)
         ("C-c C-l" . color-rg-jump-next-file))))

;; jump between texts
;; https://emacsredux.com/blog/2015/07/19/ace-jump-mode-is-dead-long-live-avy
(use-package avy
  :bind (("C-c g 2" . avy-goto-char-2)
         ("C-c g c" . avy-goto-char)
         ("C-c g e" . avy-goto-end-of-line)
         ("C-c g g" . avy-goto-char-timer)
         ("C-c g i" . avy-goto-char-in-line)
         ("C-c g j" . avy-goto-line-below)
         ("C-c g k" . avy-goto-line-above)
         ("C-c g l" . avy-goto-line)
         ("C-c g w" . avy-goto-word-or-subword-1)
         ("C-c m c" . my-avy-copy-thing-at-point)
         (:map dired-mode-map
               (";" . avy-goto-char-2))
         (:map isearch-mode-map
               ("C-a" . avy-isearch)
               ("C-'" . avy-isearch)))
  :custom (avy-style 'at-full)
  :config
  (defun my-avy-copy-thing-at-point ()
    "Copy thing at point."
    (interactive)
    (save-excursion
      (avy-goto-word-or-subword-1)
      (let ((thing (cl-case (read-char
                             (format
                              "Copy thing at point (%s: word %s: symbol %s: list %s: url): "
                              (propertize "w" 'face 'error)
                              (propertize "s" 'face 'error)
                              (propertize "l" 'face 'error)
                              (propertize "u" 'face 'error)))
                     (?w 'word)
                     (?s 'symbol)
                     (?l 'list)
                     (?u 'url))))
        (kill-new (thing-at-point thing))
        (message "%s copied." thing)))))

(use-package avy-zh
  :after avy
  :config (global-avy-zh-mode +1))

(use-package expand-region
  :bind ("C-c e ;" . er/expand-region))

(use-package ace-window
  :bind (([remap other-window] . ace-window)
         ("C-c w s" . ace-swap-window)
         ("C-c w d" . ace-delete-window)
         ("C-c w o" . ace-delete-other-windows))
  :config
  ;; Inherits from `avy'
  (with-eval-after-load 'avy
    (setq aw-keys avy-keys)
    (setq aw-background avy-background)))

(use-package winum
  :config
  (setq winum-format "%s ")
  (setq winum-mode-line-position 0)
  (winum-mode +1))

(use-package eyebrowse
  :hook (after-init . eyebrowse-mode)
  :custom (eyebrowse-keymap-prefix (kbd "C-c w"))
  :bind ("C-c w n" . eyebrowse-create-named-window-config))

(provide 'init-edit)

;;; init-edit.el ends here
