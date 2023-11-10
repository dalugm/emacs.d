;;; init-vertico.el --- vertico configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  vertico + orderless + consult + embark
;;

;;; Code:

(use-package vertico
  :hook (after-init . vertico-mode)
  :custom (vertico-cycle t))

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  (defun my--orderless-regexp (str)
    "Enhance `orderless-regexp' when searching STR."
    (require 'zh-lib)
    (setf (car str) (zh-lib-build-regexp-string (car str)))
    str)
  (advice-add 'orderless-regexp :filter-args #'my--orderless-regexp))

(use-package consult
  :init
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add 'register-preview :override #'consult-register-window)
  :bind
  (;; Remap bindings.
   ([remap bookmark-jump] . consult-bookmark)
   ([remap goto-line] . consult-goto-line)
   ([remap imenu] . consult-imenu)
   ([remap load-theme] . consult-theme)
   ([remap locate] . consult-locate)
   ([remap man] . consult-man)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap recentf-open-files] . consult-recent-file)
   ([remap repeat-complex-command] . consult-complex-command)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap yank-pop] . consult-yank-pop)
   ;; Register access.
   ([remap abbrev-prefix-mark] . consult-register-store)
   ("M-#" . consult-register-load)
   ("C-M-#" . consult-register)
   ;; C-c bindings in `mode-specific-map'.
   ("C-c M-x" . consult-mode-command)
   ("C-c s I" . consult-imenu-multi)
   ("C-c s f" . consult-fd)
   ("C-c s F" . consult-find)
   ("C-c s L" . consult-locate)
   ("C-c s g" . consult-grep)
   ("C-c s v" . consult-git-grep)
   ("C-c s r" . consult-ripgrep)
   ("C-c s l" . consult-line)
   ("C-c s m" . consult-line-multi)
   ("C-c s u" . consult-focus-lines)
   ("C-c s k" . consult-keep-lines)
   ;; M-g bindings in `goto-map'.
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ;; M-s bindings in `search-map'.
   ("M-s d" . consult-fd)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Other bindings.
   ("M-L" . consult-line)
   (:map minibuffer-local-map
         ("M-h" . consult-history)
         ([remap next-matching-history-element] . consult-history)
         ([remap previous-matching-history-element] . consult-history))
   (:map isearch-mode-map
         ("M-h" . consult-isearch-history)
         ("M-l" . consult-line)
         ("M-m" . consult-line-multi)
         ([remap isearch-edit-string] . consult-isearch-history)))
  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; Relevant when use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (consult-narrow-key "<")
  ;; Optionally configure the register formatting. This improves the
  ;; register preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  ;; Use Consult to select xref locations with preview.
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  (defcustom my-consult-zh-prefix ?:
    "The prefix character when using consult to search Zhongwen."
    :group 'convenience
    :type 'character)

  (defun my--consult-zh-regexp-compiler (input type ignore-case)
    "Compile the INPUT string to a list of regular expressions.

The function should return a pair, the list of regular expressions and a
highlight function. The highlight function should take a single
argument, the string to highlight given the INPUT. TYPE is the desired
type of regular expression, which can be `basic', `extended', `emacs' or
`pcre'. If IGNORE-CASE is non-nil return a highlight function which
matches case insensitively."
    (require 'zh-lib)
    (setq input (consult--split-escaped
                 (if (char-equal my-consult-zh-prefix (string-to-char input))
                     ;; Detect the first entered character. If it
                     ;; matches `my-consult-zh-prefix', convert the
                     ;; subsequent characters into Zhongwen regexp.
                     (zh-lib-build-regexp-string (substring input 1))
                   input)))
    (cons (mapcar (lambda (x) (consult--convert-regexp x type)) input)
          (when-let (regexps (seq-filter #'consult--valid-regexp-p input))
            (apply-partially #'consult--highlight-regexps regexps ignore-case))))

  (advice-add 'consult--default-regexp-compiler :override #'my--consult-zh-regexp-compiler)

  (when my-win-p
    (defun my--consult-find-win (&optional dir initial)
      "Use `consult-find' on Windows.

URL `https://github.com/minad/consult/issues/475'."
      (pcase-let* ((w32-quote-process-args ?\\) ; or (w32-quote-process-args ?*)
                   (consult-find-args (string-join
                                       (push find-program (cdr (string-split consult-find-args)))
                                       " "))
                   (`(,prompt ,paths ,dir) (consult--directory-prompt "Find" dir))
                   (default-directory dir)
                   (builder (consult--find-make-builder paths)))
        (find-file (consult--find prompt builder initial))))
    (advice-add 'consult-find :override #'my--consult-find-win))

  (consult-customize
   consult-theme :preview-key '(:debounce 0.5 any)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-vertico)

;;; init-vertico.el ends here
