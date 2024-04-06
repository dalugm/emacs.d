;;; init-misc.el --- misc config for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; misc configuration.
;;

;;; Code:

(use-package epg
  :defer t
  :custom (epg-pinentry-mode 'loopback))

(use-package fortune
  :defer t
  :custom
  (fortune-dir
   (cond
    (my-mac-p "/usr/local/Cellar/fortune/9708/share/games/fortunes/")
    (my-linux-p "/usr/share/games/fortunes/")
    (t "~/fortunes/")))
  (fortune-file (expand-file-name "." fortune-dir)))

(use-package calendar
  :defer t
  :custom (calendar-chinese-all-holidays-flag t))

(use-package holidays
  :defer t
  :custom
  (holiday-local-holidays
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
     (holiday-fixed 12 26 "Mao's Birthday"))))

(defcustom my-http-proxy "127.0.0.1:1080"
  "HTTP proxy."
  :group 'convenience
  :type 'string)

(defcustom my-socks-proxy
  (list
   (if my-wsl-p
       (if (file-exists-p "/etc/resolv.conf")
           (shell-command-to-string
            "cat /etc/resolv.conf | grep nameserver | awk '{ printf $2 }'")
         "0.0.0.0")
     "127.0.0.1")
   1080)
  "SOCKS proxy."
  :group 'convenience
  :type '(list (string :tag "Host")
               (integer :tag "Port")))

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

(when my-win-p
  (defcustom my-use-gbk-dos-coding-system nil
    "Non-nil means using GBK-DOS coding system."
    :group 'convenience
    :type 'boolean)

  (when my-use-gbk-dos-coding-system
    (add-to-list 'process-coding-system-alist
                 '("[rR][gG]" . (utf-8 . gbk-dos)))))

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

(use-package xiaoshuo-mode
  :bind ("C-c t x" . xiaoshuo-mode))

(provide 'init-misc)
;;; init-misc.el ends here
