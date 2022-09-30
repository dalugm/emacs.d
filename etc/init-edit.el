;;; init-edit.el --- edit in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Emacs is the best editor ever.
;;

;;; Code:

(use-package tree-sitter
  :hook ((after-init . global-tree-sitter-mode)
         (tree-sitter-after-on . tree-sitter-hl-mode))
  :config
  ;; Add Emacs-Lisp for tree-sitter:
  ;;
  ;; 1. git clone https://github.com/Wilfred/tree-sitter-elisp
  ;; 2. gcc -shared -fPIC -g -O2 -I./src src/parser.c -o elisp.so
  ;; 3. cp ./elisp.so ~/.tree-sitter/bin
  ;; (~/.tree-sitter/bin is inside the `tree-sitter-load-path')
  ;;
  ;; add other language support as shown above
  ;; if tree-sitter-lang include src/scanner.c or src/scanner.cc
  ;; execute the following cmd.
  ;; g++ -shared -fPIC -fno-exceptions -g -O2 -static-libgcc -static-libstdc++ -I./src src/scanner.cc -xc src/parser.c -o elisp.so
  ;; Check `tree-sitter-langs-compile' for more details

  (tree-sitter-require 'latex)
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(latex-mode . latex))

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

               ("C-w" . grammatical-edit-backward-delete)
               ;; remap `delete-char'
               ("C-d" . grammatical-edit-forward-delete)
               ("C-M-w" . grammatical-edit-backward-kill)
               ("C-M-k" . grammatical-edit-kill)

               ("C-c e l" . grammatical-edit-jump-right)
               ("C-c e h" . grammatical-edit-jump-left)
               ("C-c e n" . grammatical-edit-jump-out-pair-and-newline)
               ("C-c e j" . grammatical-edit-jump-up))))

(use-package color-rg
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
  :hook (after-init . avy-setup-default)
  :bind (("C-c g 2" . avy-goto-char-2)
         ("C-c g c" . avy-goto-char)
         ("C-c g e" . avy-goto-end-of-line)
         ("C-c g g" . avy-goto-char-timer)
         ("C-c g i" . avy-goto-char-in-line)
         ("C-c g j" . avy-goto-line-below)
         ("C-c g k" . avy-goto-line-above)
         ("C-c g l" . avy-goto-line)
         ("C-c g w" . avy-goto-word-or-subword-1)
         ("C-c m c" . my-avy-copy-thing-at-point))
  :custom (avy-style 'at-full)
  :config
  (defun my-avy-copy-thing-at-point ()
    "Copy thing at point."
    (interactive)
    (save-excursion
      (avy-goto-word-or-subword-1)
      (let ((thing
             (cl-case
                 (read-char
                  (format
                   "Copy thing at point (%s: word %s: symbol %s: list %s: url): "
                   (propertize "w" 'face 'error)
                   (propertize "s" 'face 'error)
                   (propertize "l" 'face 'error)
                   (propertize "u" 'face 'error)))
               (?w  'word)
               (?s  'symbol)
               (?l  'list)
               (?u  'url))))
        (kill-new (thing-at-point thing))
        (message "%s copied." thing)))))

(use-package expand-region
  :bind ("C-c ;" . er/expand-region))

(provide 'init-edit)

;;; init-edit.el ends here
