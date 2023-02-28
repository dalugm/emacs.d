;;; init-prog.el --- Programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Generic Programming Configuration.
;;

;;; Code:

(use-package treesit
  :when (treesit-available-p)
  :init
  (setq treesit-language-source-alist
        '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash.git"))
          (c          . ("https://github.com/tree-sitter/tree-sitter-c.git"))
          (cmake      . ("https://github.com/uyha/tree-sitter-cmake.git"))
          (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp.git"))
          (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
          (css        . ("https://github.com/tree-sitter/tree-sitter-css.git"))
          (dart       . ("https://github.com/UserNobody14/tree-sitter-dart.git"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile.git"))
          (go         . ("https://github.com/tree-sitter/tree-sitter-go.git"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html.git"))
          (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript.git"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json.git"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python.git"))
          (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby.git"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust.git"))
          (toml       . ("https://github.com/tree-sitter/tree-sitter-toml.git"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "typescript/src"))
          (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml.git"))))
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
                  (dockerfile . (,(rx (or (seq "Dockerfile" (opt "." (zero-or-more nonl))) (seq "." (any "Dd") "ockerfile")) eos) . dockerfile-ts-mode))
                  (go         . (,(rx ".go" eos) . go-ts-mode))
                  (gomod      . (,(rx "/go.mod" eos) . go-mod-ts-mode))
                  (rust       . (,(rx ".rs" eos) . rust-ts-mode))
                  (tsx        . (,(rx ".tsx" eos) . tsx-ts-mode))
                  (typescript . (,(rx ".ts" eos) . typescript-ts-mode))
                  (yaml       . (,(rx ".y" (opt "a") "ml" eos) . yaml-ts-mode))))
    (let ((parser (car list))
          (alist (cdr list)))
      (when (treesit-ready-p parser)
        (add-to-list 'auto-mode-alist alist)))))

(defvar my-last-compilation-buffer nil
  "The last buffer in which compilation took place.")

(with-eval-after-load 'compile
  ;; Save before compiling.
  (setq compilation-ask-about-save nil)
  ;; Kill old compile processes before starting the new one.
  (setq compilation-always-kill t)
  ;; Automatically scroll to first error.
  (setq compilation-scroll-output 'first-error)

  (defun my--save-compilation-buffer (&rest _)
    "Save the compilation buffer to find it later."
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

(keymap-global-set "C-c c k" #'compile)
(keymap-global-set "C-c c r" #'recompile)

;; Colorize output of Compilation Mode.
;; https://stackoverflow.com/a/3072831/355252
(with-eval-after-load 'compile
  (require 'ansi-color)

  (defun my--colorize-compilation-buffer ()
    "Colorize a compilation mode buffer."
    ;; We don't want to mess with child modes such as grep-mode, ack, ag, etc.
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max)))))

  (add-hook 'compilation-filter-hook #'my--colorize-compilation-buffer))

(defun my--generic-prog-mode-hook-setup ()
  "Generic configuration for `prog-mode'."
  ;; Camel case aware editing operations.
  (subword-mode +1))

(add-hook 'prog-mode-hook #'my--generic-prog-mode-hook-setup)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package xml-mode
  :mode "\\.[^.]*proj\\'"
  :mode "\\.xaml\\'"
  :mode "\\.p\\(?:list\\|om\\)\\'"
  :mode "\\.xs\\(?:d\\|lt\\)\\'"
  :mode "\\.rss\\'")

(use-package evil-nerd-commenter
  :bind (("C-c c l" . evilnc-comment-or-uncomment-lines)
         ("C-c c d" . evilnc-copy-and-comment-lines)
         ("C-c c p" . evilnc-comment-or-uncomment-paragraphs)))

(use-package editorconfig
  :hook (prog-mode . editorconfig-mode)
  :bind ("C-c c E" . editorconfig-apply))

(use-package apheleia
  :bind ("C-c c F" . apheleia-format-buffer)
  :config
  (add-to-list 'apheleia-mode-alist '(emacs-lisp-mode . lisp-indent)))

(use-package citre
  :init
  (require 'citre-config)
  (setq citre-auto-enable-citre-mode-modes '(prog-mode))
  (defun my-citre-jump ()
    "Fallback to `xref' when citre failed."
    (interactive)
    (condition-case _
        (citre-jump)
      (error (call-interactively #'xref-find-definitions))))
  (defun my-citre-jump-back ()
    "Fallback to `xref' when citre failed."
    (interactive)
    (condition-case _
        (citre-jump-back)
      (error (call-interactively #'xref-pop-marker-stack))))
  :bind (("C-c c a" . citre-ace-peek)
         ("C-c c e" . citre-edit-tags-file-recipe)
         ("C-c c h" . citre-peek)
         ("C-c c t" . citre-update-this-tags-file)
         ("C-c c j" . my-citre-jump)
         ("C-c c J" . my-citre-jump-back)))

(use-package ggtags
  :bind (("C-c c g m" . ggtags-mode)
         ("C-c c g c" . ggtags-create-tags)
         ("C-c c g u" . ggtags-update-tags)
         ("C-c c g b" . ggtags-browse-file-as-hypertext)
         ("C-c c g d" . ggtags-find-definition)
         ("C-c c g r" . ggtags-find-reference)
         ("C-c c g s" . ggtags-find-other-symbol)
         ("C-c c g g" . ggtags-find-tag-dwim)
         ("C-c c g D" . ggtags-delete-tags)))

(provide 'init-prog)

;;; init-prog.el ends here
