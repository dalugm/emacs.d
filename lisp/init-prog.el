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
          '((bash "https://github.com/tree-sitter/tree-sitter-bash")
            (c "https://github.com/tree-sitter/tree-sitter-c")
            (c3 "https://github.com/c3lang/tree-sitter-c3")
            (clojure "https://github.com/sogaiu/tree-sitter-clojure")
            (cmake "https://github.com/uyha/tree-sitter-cmake")
            (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
            (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
            (css "https://github.com/tree-sitter/tree-sitter-css")
            (dart "https://github.com/UserNobody14/tree-sitter-dart")
            (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
            (doxygen "https://github.com/tree-sitter-grammars/tree-sitter-doxygen")
            (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
            (go "https://github.com/tree-sitter/tree-sitter-go")
            (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
            (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
            (heex "https://github.com/phoenixframework/tree-sitter-heex")
            (html "https://github.com/tree-sitter/tree-sitter-html")
            (java "https://github.com/tree-sitter/tree-sitter-java")
            (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
            (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
            (json "https://github.com/tree-sitter/tree-sitter-json")
            (just "https://github.com/IndianBoy42/tree-sitter-just")
            (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
            (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src")
            (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown-inline/src")
            (nix "https://github.com/nix-community/tree-sitter-nix")
            (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" nil "grammars/ocaml/src")
            (ocaml-interface "https://github.com/tree-sitter/tree-sitter-ocaml" nil "grammars/interface/src")
            (odin "https://github.com/tree-sitter-grammars/tree-sitter-odin")
            (php "https://github.com/tree-sitter/tree-sitter-php" nil "php/src")
            (phpdoc "https://github.com/claytonrcarter/tree-sitter-phpdoc")
            (purescript "https://github.com/postsolar/tree-sitter-purescript")
            (python "https://github.com/tree-sitter/tree-sitter-python")
            (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
            (rust "https://github.com/tree-sitter/tree-sitter-rust")
            (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml")
            (tsx "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
            (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
            (typst "https://github.com/uben0/tree-sitter-typst")
            (vue "https://github.com/tree-sitter-grammars/tree-sitter-vue")
            (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
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
     (mhtml-mode      . mhtml-ts-mode)
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
                  (tsx        . (,(rx "." (any "jt") "sx" eos) . tsx-ts-mode))
                  (typescript . (,(rx ".ts" eos) . typescript-ts-mode))
                  (yaml       . (,(rx ".y" (opt "a") "ml" eos) . yaml-ts-mode))))
    (let ((parser (car list))
          (alist (cdr list)))
      (when (treesit-ready-p parser 'message)
        (add-to-list 'auto-mode-alist alist)))))

(use-package eglot
  :bind (("C-c l l" . eglot)
         ("C-c l a" . eglot-code-actions)
         ("C-c l c" . eglot-show-workspace-configuration)
         ("C-c l d" . eglot-find-declaration)
         ("C-c l f" . eglot-format)
         ("C-c l h" . eldoc)
         ("C-c l i" . eglot-find-implementation)
         ("C-c l n" . eglot-rename)
         ("C-c l q" . eglot-shutdown)
         ("C-c l t" . eglot-find-typeDefinition)
         ("C-c l R" . eglot-reconnect)
         ("C-c l Q" . eglot-shutdown-all)))

(use-package eglot-booster
  :when (executable-find "emacs-lsp-booster")
  :after eglot
  :custom (eglot-booster-io-only t)
  :config (eglot-booster-mode +1))

(use-package eglot-tempel
  :after (eglot tempel)
  :config (eglot-tempel-mode +1))

(use-package compile
  :bind (("C-c c k" . compile)
         ("C-c c r" . recompile))
  :custom
  (compilation-ask-about-save nil)
  (compilation-always-kill t)
  (compilation-scroll-output 'first-error)
  :hook
  (compilation-filter . ansi-color-compilation-filter))

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
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff))
  (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
  (add-to-list 'apheleia-mode-alist '(vue-ts-mode . prettier))

  (setf (alist-get 'rustfmt apheleia-formatters)
        '("rustfmt" "--edition" "2024" "--quiet" "--emit" "stdout"))
  (setf (alist-get 'google-java-format apheleia-formatters)
        '("google-java-format" "--aosp" "-")))

;;;; Major modes.

(use-package js
  :mode ("\\.[cm]js\\'" . js-mode)
  :custom (js-indent-level 2))

(use-package python
  :mode ("\\.[cir]py\\'" . python-mode)
  :custom
  (python-indent-guess-indent-offset nil)
  (python-indent-offset 4))

(use-package tex-mode
  :defer t
  :config
  (setopt tex-command "xelatex")
  (add-to-list 'tex-compile-commands '("xelatex %f" t "%r.pdf")))

(use-package fsharp-mode :defer t)
(use-package purescript-mode :defer t)

(use-package c3-ts-mode
  :when (treesit-available-p)
  :mode "\\.c3\\'")

(use-package dart-ts-mode
  :when (treesit-available-p)
  :mode "\\.dart\\'")

(use-package odin-ts-mode
  :when (treesit-available-p)
  :mode "\\.odin\\'")

(use-package haskell-ts-mode
  :when (treesit-available-p)
  :bind ("C-c C-z" . run-haskell)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(haskell-ts-mode
                   . ("haskell-language-server-wrapper" "--lsp"))))
  :mode "\\.hs\\'")

(use-package neocaml
  :when (treesit-available-p)
  :hook (neocaml-mode . neocaml-repl-minor-mode)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((neocaml-mode :language-id "ocaml") . ("ocamllsp"))))
  :mode
  (("\\.mli\\'" . neocamli-mode)
   ("\\.ml\\'" . neocaml-mode)))

(use-package ocaml-eglot
  :after (eglot neocaml)
  :config (ocaml-eglot +1))

(use-package just-ts-mode
  :when (treesit-available-p)
  :mode "\\.[Jj]ust\\(file\\)?\\'")

(use-package nix-ts-mode
  :when (treesit-available-p)
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

(use-package vue-ts-mode
  :when (treesit-available-p)
  :config
  (with-eval-after-load 'eglot
    ;; Eglot with vuels.
    (add-to-list 'eglot-server-programs
                 '(vue-ts-mode . (eglot-vuels "vue-language-server" "--stdio")))

    (defclass eglot-vuels (eglot-lsp-server) ()
      :documentation "vue-language-server")

    (cl-defmethod eglot-initialization-options ((server eglot-vuels))
      "Pass through required cquery initialization options"
      (let* ((get-ts-root
              (lambda (&optional global)
                (let* ((pnpm-root-cmd (format "pnpm root %s" (if global "--global" "")))
                       (node-modules-dir (string-trim-right (shell-command-to-string pnpm-root-cmd)))
                       (ts-dir (expand-file-name "typescript" node-modules-dir)))
                  (when (file-exists-p ts-dir)
                    ts-dir))))
             (ts-package-path (or (funcall get-ts-root) (funcall get-ts-root t)))
             (tsdk-path (and ts-package-path (expand-file-name "lib" ts-package-path))))
        (when tsdk-path
          `( :typescript (:tsdk ,tsdk-path)
             :vue (:hybridMode :json-false))))))
  :mode "\\.[nu]?vue\\'")

(use-package zig-ts-mode
  :when (treesit-available-p)
  :mode "\\.zig\\'")

(provide 'init-prog)
;;; init-prog.el ends here
