;;; init-edit.el --- edit in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Emacs is the best editor ever.
;;

;;; Code:

(use-package ediff
  :defer t
  :custom
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package project
  :defer t
  :config
  (require 'keymap)
  (require 'cl-seq)

  (keymap-substitute project-prefix-map #'project-find-regexp #'consult-ripgrep)
  (cl-nsubstitute-if
   '(consult-ripgrep "Find regexp")
   (pcase-lambda (`(,cmd _)) (eq cmd #'project-find-regexp))
   project-switch-commands)

  (keymap-substitute project-prefix-map #'project-find-file #'consult-fd)
  (cl-nsubstitute-if
   '(consult-fd "Find file")
   (pcase-lambda (`(,cmd _)) (eq cmd #'project-find-file))
   project-switch-commands)

  (keymap-set project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

(use-package tab-bar
  :defer t
  :custom
  ;; Always keep the tab bar hidden.
  (tab-bar-show nil)
  (tab-bar-new-tab-choice "*scratch*"))

(use-package zh-lib
  :defer t
  :custom (zh-lib-scheme 'simplified-traditional-quanpin-all))

(use-package marginalia
  :hook (after-init . marginalia-mode))

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
  ;; Optionally replace the key help with a completing-read interface.
  (prefix-help-command #'embark-prefix-help-command)
  :config
  (defun my-embark-preview ()
    "Previews candidate in vertico buffer, unless it's a consult command"
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim)))))

  ;; Hide the mode line of the Embark live/completions buffers.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :config
  ;; Integrate workspace buffers into `consult-buffer'.
  (with-eval-after-load 'consult
    (defvar consult--source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))
      "Workspace buffer candidate source for `consult-buffer'.")

    (defun my--consult-tabspaces ()
      "Isolate workspace buffers when using tabspaces."
      (if tabspaces-mode
          (add-to-list 'consult-buffer-sources 'consult--source-workspace)
        ;; Reset `consult-buffer' to show all buffers.
        (setq consult-buffer-sources
              (remove #'consult--source-workspace consult-buffer-sources))))

    (my--consult-tabspaces)
    (add-hook 'tabspaces-mode-hook #'my--consult-tabspaces)))

(use-package ace-window
  :bind (([remap other-window] . ace-window)
         ("C-c w s" . ace-swap-window)
         ("C-c w d" . ace-delete-window)
         ("C-c w o" . ace-delete-other-windows))
  :config
  ;; Inherits from `avy'.
  (with-eval-after-load 'avy
    (setopt aw-keys avy-keys
            aw-background avy-background)))

(use-package winum
  :hook (after-init . winum-mode)
  :custom
  (winum-format "%s ")
  (winum-mode-line-position 0))

;; Jump between texts.
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
               :package dired-mode
               (";" . avy-goto-char-2))
         (:map isearch-mode-map
               ("C-a" . avy-isearch)
               ("C-'" . avy-isearch)))
  :custom (avy-style 'at-full)
  :init
  (defun my-avy-copy-thing-at-point ()
    "Copy thing at point using `avy'."
    (interactive)
    (save-excursion
      (avy-goto-word-or-subword-1)
      (kill-new (thing-at-point
                 (cl-case (read-char "w: word, s: symbol, l: list, u: url")
                   (?w 'word)
                   (?s 'symbol)
                   (?l 'list)
                   (?u 'url))))))
  :config
  (require 'avy-zh)
  (avy-zh-mode +1))

(use-package expreg
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)
         ("C-c e ;" . expreg-expand)
         ("C-c e '" . expreg-contract)))

(use-package vundo
  :bind ("C-c e u" . vundo))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  :config
  (define-advice orderless-regexp (:filter-args (str) enhance)
    "Enhance `orderless-regexp' when searching STR."
    (require 'zh-lib)
    (setf (car str) (zh-lib-build-regexp-string (car str)))
    str))

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

  (define-advice consult--default-regexp-compiler (:override (input type ignore-case) zh)
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
          (when-let* ((regexps (seq-filter #'consult--valid-regexp-p input)))
            (apply-partially #'consult--highlight-regexps regexps ignore-case))))

  (when my-win-p
    (define-advice consult-find (:override (&optional dir initial) win)
      "Use `consult-find' on Windows.

URL `https://github.com/minad/consult/issues/475'."
      (pcase-let* ((w32-quote-process-args ?\\) ; or (w32-quote-process-args ?*)
                   (consult-find-args (string-join
                                       (push find-program (cdr (string-split consult-find-args)))
                                       " "))
                   (`(,prompt ,paths ,dir) (consult--directory-prompt "Find" dir))
                   (default-directory dir)
                   (builder (consult--find-make-builder paths)))
        (find-file (consult--find prompt builder initial)))))

  (consult-customize
   consult-theme :preview-key '(:debounce 0.5 any)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-edit)
;;; init-edit.el ends here
