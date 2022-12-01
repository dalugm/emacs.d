;;; init-misc.el --- misc config for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; misc configuration.
;;

;;; Code:

;; Show fortune in Emacs.
(with-eval-after-load 'fortune
  (when (or my-mac-p my-linux-p)
    (let ((fortune
           (cond
            (my-mac-p "/usr/local/Cellar/fortune/9708/share/games/fortunes")
            (my-linux-p "/usr/share/games/fortunes"))))
      (setq fortune-file fortune))))

(defcustom my-http-proxy "127.0.0.1:1087"
  "HTTP proxy."
  :group 'convenience
  :type 'string)

(defcustom my-socks-proxy "127.0.0.1:1080"
  "SOCKS proxy."
  :group 'convenience
  :type 'string)

(defcustom my-wsl-socks-proxy
  (concat
   (if (file-exists-p "/etc/resolv.conf")
       (shell-command-to-string
        "cat /etc/resolv.conf | grep nameserver | awk '{ printf $2 }'")
     "0.0.0.0")
   ":"
   "10810")
  "SOCKS proxy in WSL."
  :group 'convenience
  :type 'string)

(defcustom my-run-emacs-as-a-server nil
  "Non-nil means to run Emacs as a server process, which allows
access from `emacsclient'."
  :group 'convenience
  :type 'boolean)

(when my-run-emacs-as-a-server
  (run-with-idle-timer 3 nil
                       (lambda ()
                         "Run Emacs as a server process."
                         (require 'server)
                         (unless (server-running-p)
                           (message "Starting a server...")
                           (server-start)))))

;; Calendar.
(setq calendar-chinese-all-holidays-flag t)
(setq holiday-local-holidays
      `((holiday-fixed 3 8  "Women's Day")
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
  :when my-mac-x-p
  :init (setq exec-path-from-shell-check-startup-files nil)
  :config (exec-path-from-shell-initialize)
  ;; https://emacs.stackexchange.com/questions/10822/locale-when-launching-emacs-app-on-os-x
  (exec-path-from-shell-copy-envs
   '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"
     "NIX_SSL_CERT_FILE" "NIX_PATH" "GTAGSLABEL" "GTAGSCONF"
     "LANG" "LC_CTYPE" "PATH" "MANPATH"))
  (when (executable-find "gls")
    ;; Use GNU ls as `gls' from `coreutils' if available.
    (setq insert-directory-program "gls")))

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
          ;; For problems that will become bigger problems later
          ;; if not fixed ASAP.
          ("FIXME" error bold)
          ;; For problems that need to pay attention especially.
          ("WARNING" error bold)
          ;; For tidbits that are unconventional and not intended uses of
          ;; the constituent parts, or modify function for own use, and
          ;; may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For things that were done for temporarily use,
          ;; it will be removed in the future.
          ("TEMP" font-lock-keyword-face bold)
          ;; For things that were done hastily and/or hasn't been
          ;; thoroughly tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ;; For codes that need to refactor or optimize later.
          ("XXX" font-lock-keyword-face bold)
          ;; For things that has abandoned but should not removed.
          ("ABANDONED" font-lock-doc-face bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package darkroom
  :bind (("C-c t d" . darkroom-tentative-mode)
         ("C-c t D" . darkroom-mode)))

(use-package separedit
  :bind ("C-c e e" . separedit)
  :custom (separedit-remove-trailing-spaces-in-comment t)
  :init
  (defun my-eval-last-sexp-in-comment ()
    "Eval last sexp in comment by using `separedit'."
    (interactive)
    (require 'separedit)
    (let ((separedit-default-mode 'emacs-lisp-mode)
          (separedit-inhibit-edit-window-p t))
      (with-current-buffer (separedit)
        (unwind-protect (call-interactively #'eval-last-sexp)
          (separedit-abort))))))

(use-package search-dired
  :bind (("C-c s d" . search-dired-dwim)
         ("C-c s D" . search-dired)))

(provide 'init-misc)

;;; init-misc.el ends here
