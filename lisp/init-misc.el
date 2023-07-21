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

(use-package hl-todo
  :hook (after-init . global-hl-todo-mode)
  :bind (:map hl-todo-mode-map
              ("M-s M-o" . hl-todo-occur))
  :custom
  (hl-todo-highlight-punctuation ":"))

(use-package darkroom
  :bind (("C-c t d" . darkroom-tentative-mode)
         ("C-c t D" . darkroom-mode)))

(use-package separedit
  :bind ("C-c e e" . separedit)
  :custom
  (separedit-remove-trailing-spaces-in-comment t)
  (separedit-preserve-string-indentation t)
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
