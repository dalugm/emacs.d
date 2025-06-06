;;; init-prog.el --- Programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Generic Programming Configuration.
;;

;;; Code:

(use-package treesit
  :when (treesit-available-p)
  :init
  (setopt treesit-language-source-alist
          '((c3 "https://github.com/c3lang/tree-sitter-c3")
            (clojure "https://github.com/sogaiu/tree-sitter-clojure")
            (dart "https://github.com/UserNobody14/tree-sitter-dart")
            (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
            (just "https://github.com/IndianBoy42/tree-sitter-just")
            (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src")
            (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown-inline/src")
            (nix "https://github.com/nix-community/tree-sitter-nix")
            (odin "https://github.com/tree-sitter-grammars/tree-sitter-odin")
            (rust "https://github.com/tree-sitter/tree-sitter-rust")
            (typst "https://github.com/uben0/tree-sitter-typst")
            (zig "https://github.com/maxxnino/tree-sitter-zig")))
  :custom
  (major-mode-remap-alist
   '((c-mode          . c-ts-mode)
     (c++-mode        . c++-ts-mode)
     (c-or-c++-mode   . c-or-c++-ts-mode)
     (conf-toml-mode  . toml-ts-mode)
     (csharp-mode     . csharp-ts-mode)
     (css-mode        . css-ts-mode)
     (html-mode       . html-ts-mode)
     (java-mode       . java-ts-mode)
     (javascript-mode . js-ts-mode)
     (js-json-mode    . json-ts-mode)
     (js-mode         . js-ts-mode)
     (python-mode     . python-ts-mode)
     (ruby-mode       . ruby-ts-mode)
     (sh-mode         . bash-ts-mode)))
  :config
  ;; Add `*-ts-mode' to `auto-mode-alist'.
  (dolist (list `((cmake      . (,(rx (or "CMakeLists.txt" ".cmake") eos) . cmake-ts-mode))
                  (dockerfile . (,(rx "Dockerfile" eos) . dockerfile-ts-mode))
                  (elixir     . (,(rx (or ".elixir" (seq ".ex" (opt "s")) "mix.lock") eos) . elixir-ts-mode))
                  (go         . (,(rx ".go" eos) . go-ts-mode))
                  (gomod      . (,(rx "/go.mod" eos) . go-mod-ts-mode))
                  (heex       . (,(rx "." (opt (any "hl")) "eex" eos) . heex-ts-mode))
                  (lua        . (,(rx ".lua" eos) . lua-ts-mode))
                  (tsx        . (,(rx "." (opt (any "jt")) "sx" eos) . tsx-ts-mode))
                  (typescript . (,(rx ".ts" eos) . typescript-ts-mode))
                  (yaml       . (,(rx ".y" (opt "a") "ml" eos) . yaml-ts-mode))))
    (let ((parser (car list))
          (alist (cdr list)))
      (when (treesit-ready-p parser 'message)
        (add-to-list 'auto-mode-alist alist)))))

(use-package compile
  :bind (("C-c c k" . compile)
         ("C-c c r" . recompile))
  :custom
  (compilation-ask-about-save nil)
  (compilation-always-kill t)
  (compilation-scroll-output 'first-error)
  :hook
  (compilation-filter . ansi-color-compilation-filter))

(use-package etags
  :defer t
  :custom (tags-revert-without-query t))

(use-package subword
  :hook ((prog-mode text-mode) . subword-mode))

(use-package eldoc-box
  :when (display-graphic-p)
  :hook ((eldoc-mode eglot-managed-mode) . eldoc-box-hover-mode)
  :custom
  (eldoc-box-only-multi-line t)
  (eldoc-box-clear-with-C-g t)
  (eldoc-box-use-visible-frame-map t)
  :bind (("C-c h h" . eldoc-box-help-at-point)
         (:map eldoc-box-visible-frame-map
               ("C-M-n" . eldoc-box-scroll-up)
               ("C-M-p" . eldoc-box-scroll-down)
               ("C-M-a" . eldoc-box-beginning)
               ("C-M-e" . eldoc-box-end)))
  :config
  (add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t)
  (setopt eldoc-doc-buffer-separator
          (concat "\n"
                  (propertize "-"
                              'display '(space :align-to right)
                              'face '(:strike-through t)
                              'font-lock-face '(:strike-through t))
                  "\n")))

(use-package evil-nerd-commenter
  :bind (("C-c c l" . evilnc-comment-or-uncomment-lines)
         ("C-c c d" . evilnc-copy-and-comment-lines)
         ("C-c c p" . evilnc-comment-or-uncomment-paragraphs)))

(use-package citre
  :bind (("C-c c a" . citre-ace-peek)
         ("C-c c e" . citre-edit-tags-file-recipe)
         ("C-c c h" . citre-peek)
         ("C-c c t" . citre-update-this-tags-file)
         ("C-c c j" . citre-jump)
         ("C-c c J" . citre-jump-back))
  :custom
  (citre-auto-enable-citre-mode-modes '(prog-mode))
  (citre-default-create-tags-file-location 'global-cache)
  :config
  ;; Add Elisp to the backend lists.
  (citre-register-backend 'elisp
                          (citre-xref-backend-to-citre-backend
                           'elisp
                           (lambda () (derived-mode-p 'emacs-lisp-mode))))
  (add-to-list 'citre-find-definition-backends 'elisp)
  (add-to-list 'citre-find-reference-backends 'elisp))

(use-package apheleia
  :bind (("C-c c f" . apheleia-format-buffer)
         ("C-c c F" . apheleia-goto-error))
  :config
  (add-to-list 'apheleia-formatters
               '(google-java-format . ("google-java-format" "--aosp" "-")))
  (add-to-list 'apheleia-formatters '(rustfmt . ("rustfmt" "--quiet"
                                                 "--edition" "2021"
                                                 "--emit" "stdout")))

  (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff))
  (add-to-list 'apheleia-mode-alist '(python-mode . ruff)))

;;;; Major modes.

(use-package js
  :mode ("\\.[cm]js\\'" . js-mode)
  :custom (js-indent-level 2))

(use-package python
  :mode ("\\.[cir]py\\'" . python-mode)
  :custom
  (python-indent-guess-indent-offset nil)
  (python-indent-offset 4))

(use-package nxml
  :mode
  ("\\.[^.]*proj\\'" . nxml-mode)
  ("\\.xaml\\'" . nxml-mode)
  ("\\.p\\(?:list\\|om\\)\\'" . nxml-mode)
  ("\\.xs\\(?:d\\|lt\\)\\'" . nxml-mode)
  ("\\.rss\\'" . nxml-mode))

(use-package tex-mode
  :defer t
  :config
  (setopt tex-command "xelatex")
  (add-to-list 'tex-compile-commands '("xelatex %f" t "%r.pdf")))

(use-package c3-ts-mode
  :when (and (treesit-available-p) (treesit-ready-p 'c3 'message))
  :mode "\\.c3\\'")

(use-package dart-ts-mode
  :when (and (treesit-available-p) (treesit-ready-p 'dart 'message))
  :mode "\\.dart\\'")

(use-package fsharp-mode :defer t)

(use-package odin-ts-mode
  :when (and (treesit-available-p) (treesit-ready-p 'odin 'message))
  :mode "\\.odin\\'")

(use-package haskell-ts-mode
  :when (and (treesit-available-p) (treesit-ready-p 'haskell 'message))
  :mode "\\.hs\\'"
  :config (with-eval-after-load 'eglot (haskell-ts-setup-eglot)))

(use-package just-ts-mode
  :when (and (treesit-available-p) (treesit-ready-p 'just 'message))
  :defer t)

(use-package nix-ts-mode
  :when (and (treesit-available-p) (treesit-ready-p 'nix 'message))
  :mode "\\.nix\\'")

(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-c C-c" . rust-compile)
              ("C-c C-c C-d" . rust-dbg-wrap-or-unwrap)
              ("C-c C-c C-m" . rust-toggle-mutability)
              ;; Unbind `rust-dbg-wrap-or-unwrap' for doc.
              ("C-c C-d" . nil)
              ("C-c C-d C-d" . my-rust-doc)
              ("C-c C-d C-o" . my-rust-doc-open)
              ("C-c C-p C-b" . rust-playpen-buffer)
              ("C-c C-p C-r" . rust-playpen-region)
              ("C-c C-r C-c" . rust-compile-release)
              ("C-c C-r C-r" . rust-run-release))
  :custom (rust-mode-treesitter-derive t)
  :config
  (defun my-rust-doc ()
    "Build documentation using `cargo doc'."
    (interactive)
    (rust--compile nil "%s doc" rust-cargo-bin))

  (defun my-rust-doc-open ()
    "Build and open documentation using `cargo doc'."
    (interactive)
    (rust--compile nil "%s doc --open" rust-cargo-bin)))

(use-package tuareg :defer t)

(use-package zig-mode
  :defer t
  :custom (zig-format-on-save nil))

(provide 'init-prog)
;;; init-prog.el ends here
