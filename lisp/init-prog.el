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
            (clojure "https://github.com/sogaiu/tree-sitter-clojure")
            (cmake "https://github.com/uyha/tree-sitter-cmake")
            (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
            (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
            (css "https://github.com/tree-sitter/tree-sitter-css")
            (dart "https://github.com/UserNobody14/tree-sitter-dart")
            (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
            (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
            (erlang "https://github.com/WhatsApp/tree-sitter-erlang")
            (go "https://github.com/tree-sitter/tree-sitter-go")
            (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
            (heex "https://github.com/phoenixframework/tree-sitter-heex")
            (html "https://github.com/tree-sitter/tree-sitter-html")
            (java "https://github.com/tree-sitter/tree-sitter-java")
            (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
            (json "https://github.com/tree-sitter/tree-sitter-json")
            (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
            (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
            (python "https://github.com/tree-sitter/tree-sitter-python")
            (racket "https://github.com/6cdh/tree-sitter-racket")
            (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
            (rust "https://github.com/tree-sitter/tree-sitter-rust")
            (toml "https://github.com/tree-sitter/tree-sitter-toml")
            (tsx "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
            (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
            (typst "https://github.com/uben0/tree-sitter-typst")
            (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
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
                  (rust       . (,(rx ".rs" eos) . rust-ts-mode))
                  (tsx        . (,(rx ".tsx" eos) . tsx-ts-mode))
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
  :config
  ;; Colorize output of Compilation Mode.
  ;; https://stackoverflow.com/a/3072831/355252
  (require 'ansi-color)

  (defun my--colorize-compilation-buffer ()
    "Colorize a compilation mode buffer."
    ;; Don't mess with child modes such as grep, ack, ag, etc.
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max)))))

  (add-hook 'compilation-filter-hook #'my--colorize-compilation-buffer)

  (defvar my-last-compilation-buffer nil
    "The last buffer in which compilation took place.")

  (defun my--save-compilation-buffer (&rest _)
    "Save the last compilation buffer to find it later."
    (setq my-last-compilation-buffer next-error-last-buffer))

  (advice-add 'compilation-start :after #'my--save-compilation-buffer)

  (defun my--find-prev-compilation (orig &optional edit-command)
    "Find the previous compilation buffer, if present, and recompile there."
    (if (and (null edit-command)
             (not (derived-mode-p 'compilation-mode))
             my-last-compilation-buffer
             (buffer-live-p (get-buffer my-last-compilation-buffer)))
        (with-current-buffer my-last-compilation-buffer
          (funcall orig edit-command))
      (funcall orig edit-command)))

  (advice-add 'recompile :around #'my--find-prev-compilation))

(use-package etags
  :defer t
  :custom (tags-revert-without-query t))

(use-package subword
  :hook ((prog-mode text-mode) . subword-mode))

(use-package eldoc-box
  :when (display-graphic-p)
  :hook (eldoc-mode . eldoc-box-hover-mode)
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
  (setopt eldoc-doc-buffer-separator
          (concat "\n"
                  (propertize "-"
                              'display '(space :align-to right)
                              'face '(:strike-through t)
                              'font-lock-face '(:strike-through t))
                  "\n")))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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
  (add-to-list 'apheleia-formatters '(rustfmt . ("rustfmt" "--quiet"
                                                 "--emit" "stdout"
                                                 "--edition" "2021"))))

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

(use-package dart-ts-mode
  :when (and (treesit-available-p) (treesit-ready-p 'dart 'message))
  :mode "\\.dart\\'")

(use-package kotlin-ts-mode
  :when (and (treesit-available-p) (treesit-ready-p 'kotlin 'message))
  :mode "\\.kts?\\'")

(provide 'init-prog)
;;; init-prog.el ends here
