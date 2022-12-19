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
  ;; Optionally configure the register formatting. This improves the
  ;; register preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add 'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :bind
  (([remap bookmark-jump] . consult-bookmark)
   ([remap goto-line] . consult-goto-line)
   ([remap imenu] . consult-imenu)
   ([remap locate] . consult-locate)
   ([remap load-theme] . consult-theme)
   ([remap man] . consult-man)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap recentf-open-files] . consult-recent-file)
   ([remap repeat-complex-command] . consult-complex-command)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap yank-pop] . consult-yank-pop)
   ;; register access
   ([remap abbrev-prefix-mark] . consult-register-store)
   ("M-#" . consult-register-load)
   ("C-M-#" . consult-register)
   ;; other short keybindings
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-L" . consult-line)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("C-c s I" . consult-imenu-multi)
   ("C-c s f" . my-consult-fd)
   ("C-c s F" . consult-find)
   ("C-c s L" . consult-locate)
   ("C-c s g" . consult-grep)
   ("C-c s v" . consult-git-grep)
   ("C-c s r" . consult-ripgrep)
   ("C-c s l" . consult-line)
   ("C-c s m" . consult-line-multi)
   ("C-c s k" . consult-focus-lines)
   ("C-c s K" . consult-keep-lines)
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

  (defcustom my-consult-fd-command "fd"
    "The default command used to run fd."
    :group 'convenience
    :type 'string)

  (defun my--consult--fd-builder (input)
    "Build command line given INPUT."
    (unless my-consult-fd-command
      (setq my-consult-fd-command
            (if (eq 0 (call-process-shell-command "fdfind"))
                "fdfind"
              "fd")))
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler
                                        arg 'extended t)))
      (when re
        (list :command (append
                        (list my-consult-fd-command
                              "--color=never" "--full-path"
                              (consult--join-regexps re 'extended))
                        opts)
              :highlight hl))))

  (defun my-consult-fd (&optional dir initial)
    "Search with `fd' for files in DIR where the content matches a regexp.
The initial input is given by the INITIAL argument.  See
`consult-find' for more details."
    (interactive "P")
    (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
           (default-directory (cdr prompt-dir)))
      (find-file (consult--find (car prompt-dir)
                                #'my--consult--fd-builder
                                initial))))

  (when my-win-p

    ;; ;; If Windows does not enable utf-8 globally in CN environment.
    ;; (add-to-list 'process-coding-system-alist
    ;;              '("[rR][gG]" . (utf-8 . gbk-dos)))

    (defun my--consult-find-win (&optional dir initial)
      "Use `consult-find' on Windows.

URL `https://github.com/minad/consult/issues/475'."
      (let* ((w32-quote-process-args ?\\) ; or (w32-quote-process-args ?*)
             (consult-find-args (concat find-program " . -not ( -wholename */.* -prune )"))
             (prompt-dir (consult--directory-prompt "Find" dir))
             (default-directory (cdr prompt-dir)))
        (find-file (consult--find
                    (car prompt-dir)
                    #'consult--find-builder
                    initial))))
    (advice-add 'consult-find :override #'my--consult-find-win))

  ;; The narrowing key.
  ;; Both `<' and `C-+' work reasonably well.
  (setq consult-narrow-key "<"))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-vertico)

;;; init-vertico.el ends here
