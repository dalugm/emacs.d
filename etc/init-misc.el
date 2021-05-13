;;; init-misc.el --- misc config for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; misc configuration.
;;

;;; Code:

;; show fortune in Emacs
(with-eval-after-load 'fortune
  (when (or sys/macp sys/linuxp)
    (let ((fortune (cond
                     (sys/macp "/usr/local/Cellar/fortune/9708/share/games/fortunes")
                     (sys/linuxp "/usr/share/games/fortunes"))))
      (setq fortune-file fortune))))

;; network proxy
(defvar my-proxy "127.0.0.1:1087"
  "Network proxy.")

;; allow access from `emacsclient'
(add-hook 'after-init-hook
  (lambda ()
    (require 'server)
    (unless (server-running-p)
      (message "Starting a server...")
      (server-start))))

;; calendar
(setq calendar-chinese-all-holidays-flag t)
(setq holiday-local-holidays `((holiday-fixed 3 8  "Women's Day")
                               (holiday-fixed 3 12 "Arbor Day")
                               ,@(cl-loop for i from 1 to 3
                                          collect `(holiday-fixed 5 ,i "International Workers' Day"))
                               (holiday-fixed 5 4  "Chinese Youth Day")
                               (holiday-fixed 6 1  "Children's Day")
                               (holiday-fixed 9 9  "Mourn of Mao's Death")
                               (holiday-fixed 9 10 "Teachers' Day")
                               ,@(cl-loop for i from 1 to 7
                                          collect `(holiday-fixed 10 ,i "National Day"))
                               (holiday-fixed 12 26 "Mao's Birthday")))

(use-package exec-path-from-shell
  :defer 1
  :when (memq window-system '(mac ns x))
  :init (setq exec-path-from-shell-check-startup-files nil)
  :config (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("LANG" "GPG_AGENT_INFO" "SSH_AUTH_SOCK"))
  (dolist (var '("PATH" "MANPATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (when (executable-find "gls")
    ;; Use GNU ls as `gls' from `coreutils' if available.
    (setq insert-directory-program "gls")))

(use-package editorconfig
  :hook (after-init . editorconfig-mode)
  :bind ("C-c e a" . editorconfig-apply))

(use-package hl-todo
  :hook (after-init . global-hl-todo-mode)
  :bind (:map hl-todo-mode-map
          ("M-s M-o" . hl-todo-occur))
  :config
  (setq hl-todo-highlight-punctuation ":")
  (setq hl-todo-keyword-faces
    `(;; For things that need to be done, just not today.
       ("TODO" warning bold)
       ;; For especially important gotchas with a given implementation.
       ("NOTE" success bold)
       ;; For problems that will become bigger problems later if not fixed ASAP.
       ("FIXME" error bold)
       ;; For problems that need to pay attention especially.
       ("WARNING" error bold)
       ;; For tidbits that are unconventional and not intended uses of the
       ;; constituent parts, or modify function for own use, and may break in a
       ;; future update.
       ("HACK" font-lock-constant-face bold)
       ;; For things that were done for temporarily use,
       ;; It will be removed in the future.
       ("TEMP" font-lock-keyword-face bold)
       ;; For things that were done hastily and/or hasn't been thoroughly
       ;; tested. It may not even be necessary!
       ("REVIEW" font-lock-keyword-face bold)
       ;; For codes that need to refactor or optimize later.
       ("XXX" font-lock-keyword-face bold)
       ;; For things that has abandoned but should not removed.
       ("ABANDONED" font-lock-doc-face bold)
       ;; For things that just gotta go and will soon be gone.
       ("DEPRECATED" font-lock-doc-face bold))))

(use-package evil-nerd-commenter
  :bind (("C-c c l" . evilnc-comment-or-uncomment-lines)
         ("C-c c d" . evilnc-copy-and-comment-lines)
         ("C-c c p" . evilnc-comment-or-uncomment-paragraphs)))

(use-package darkroom
  :bind (("C-c t d" . darkroom-tentative-mode)
         ("C-c t D" . darkroom-mode)))

(use-package separedit
  :bind ("C-c e e" . separedit)
  :init
  (defun separedit/eval-last-sexp-in-comment ()
    (interactive)
    (let ((separedit-default-mode 'emacs-lisp-mode))
      (with-current-buffer (separedit)
        (prog1 (call-interactively #'eval-last-sexp)
          (execute-kbd-macro (kbd "C-c C-k"))))))

  (dolist (map (list emacs-lisp-mode-map
                     lisp-interaction-mode-map))
    (define-key map (kbd "C-c C-r") #'separedit/eval-last-sexp-in-comment)))

(use-package search-dired
  :bind (("C-c s d" . search-dired-dwim)
         ("C-c s D" . search-dired)))

;; Add both site-lisp and its immediate subdirs to `load-path'
(let ((site-lisp-dir (expand-file-name "lib/site-lisp/"
                                       user-emacs-directory)))
  (push site-lisp-dir load-path)
  (my//add-subdirs-to-load-path site-lisp-dir))

(provide 'init-misc)

;;; init-misc.el ends here
