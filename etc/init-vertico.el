;;; init-vertico.el --- vertico configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  vertico + orderless + consult + embark + marginalia
;;

;;; Code:

(use-package vertico
  :hook (after-init . vertico-mode)
  :custom (vertico-cycle t))

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :init
  ;; configure the register formatting. Improve the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Tweak the register preview window.
  ;; Add thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple
              :override #'consult-completing-read-multiple)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :bind
  (([remap apropos-command] . consult-apropos)
   ([remap bookmark-jump] . consult-bookmark)
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
   ("C-c s I" . consult-imenu-multi)
   ("C-c s l" . consult-find)
   ("C-c s L" . consult-locate)
   ("C-c s g" . consult-grep)
   ("C-c s G" . consult-git-grep)
   ("C-c s r" . my/consult-ripgrep)
   ("C-c s R" . consult-ripgrep)
   ("C-c s l" . consult-line)
   ("C-c s L" . consult-line-multi)
   ("C-c s m" . consult-multi-occur)
   ("C-c s k" . consult-keep-lines)
   ("C-c s u" . consult-focus-lines)
   ;; minibuffer history
   (:map minibuffer-local-map
    ([remap next-matching-history-element] . consult-history)
    ([remap previous-matching-history-element] . consult-history))
   (:map isearch-mode-map
    ("M-s e" . consult-isearch-history)
    ("M-s l" . consult-line)
    ("M-s L" . consult-line-multi)
    ([remap isearch-edit-string] . consult-isearch-history)
    ([remap isearch-edit-string] . consult-isearch-history)))

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; Relevant when use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :config
  ;; ---------------------------------------------------------
  ;; customize
  ;; ---------------------------------------------------------
  (defun my/consult-ripgrep (&optional DIR)
    "Modify `consult-ripgrep' functions to search files in DIR."
    (interactive "DDirectory: ")
    (consult-ripgrep nil DIR))

  ;; The narrowing key.
  ;; Both `<' and `C-+' work reasonably well.
  (setq consult-narrow-key "<"))

(use-package marginalia
  :after consult
  :config (marginalia-mode))

(use-package embark
  :bind
  (("M-A" . embark-act)
   ("M-E" . embark-export)
   ("M-D" . embark-dwim)
   ([remap describe-bindings] . embark-bindings)
   (:map minibuffer-local-map
    ("C-c C-a" . embark-act)
    ("C-c C-o" . embark-export)
    ("C-c C-c" . embark-dwim)))
  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-vertico)

;;; init-vertico.el ends here
