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

(use-package tree-sitter
  :hook ((after-init . global-tree-sitter-mode)
         (tree-sitter-after-on . tree-sitter-hl-mode))
  :config
  ;; Add Emacs-Lisp for tree-sitter:
  ;; NOTE: `so' is also valid suffix for macOS.
  ;;
  ;; 1. git clone https://github.com/Wilfred/tree-sitter-elisp
  ;; 2. gcc -shared -fPIC -O2 -I./src src/parser.c -o elisp.so
  ;; 3. cp ./elisp.so ~/.tree-sitter/bin
  ;; (~/.tree-sitter/bin is inside the `tree-sitter-load-path')
  ;;
  ;; add other language support as shown above
  ;; if tree-sitter-lang include src/scanner.c or src/scanner.cc
  ;; execute the following cmd.
  ;; gcc -shared -fPIC -O2 -I./src src/scanner.c src/parser.c -o elisp.so
  ;; g++ -shared -fPIC -fno-exceptions -O2 -static-libgcc -static-libstdc++ -I./src src/scanner.cc -xc src/parser.c -o elisp.so
  ;; Check `tree-sitter-langs-compile' for more details
  (tree-sitter-require 'elisp)
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(lisp-interaction-mode . elisp))
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(inferior-emacs-lisp-mode . elisp))
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(emacs-lisp-mode . elisp)))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package grammatical-edit
  :after tree-sitter
  :hook (prog-mode . grammatical-edit-mode)
  :bind ((:map grammatical-edit-mode-map
               ("(" . grammatical-edit-open-round)
               ("[" . grammatical-edit-open-bracket)
               ("{" . grammatical-edit-open-curly)
               (")" . grammatical-edit-close-round)
               ("]" . grammatical-edit-close-bracket)
               ("}" . grammatical-edit-close-curly)
               ("=" . grammatical-edit-equal)

               ("%" . grammatical-edit-match-paren)
               ("\"" . grammatical-edit-double-quote)
               ("'" . grammatical-edit-single-quote)

               ("SPC" . grammatical-edit-space)
               ("RET" . grammatical-edit-newline)

               ("M-\"" . grammatical-edit-wrap-double-quote)
               ("M-'" . grammatical-edit-wrap-single-quote)
               ("M-]" . grammatical-edit-wrap-bracket)
               ("M-}" . grammatical-edit-wrap-curly)
               ("M-)" . grammatical-edit-wrap-round)
               ("M-(" . grammatical-edit-unwrap)

               ("M-o" . grammatical-edit-backward-delete)
               ;; remap `delete-char'
               ("C-d" . grammatical-edit-forward-delete)
               ("C-M-w" . grammatical-edit-backward-kill)
               ("C-M-k" . grammatical-edit-kill)

               ("C-c e l" . grammatical-edit-jump-right)
               ("C-c e h" . grammatical-edit-jump-left)
               ("C-c e n" . grammatical-edit-jump-out-pair-and-newline)
               ("C-c e j" . grammatical-edit-jump-up))))

(use-package color-rg
  :custom (color-rg-recenter-match-line t)
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
  :bind ("C-c ;" . er/expand-region))

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
