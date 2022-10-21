;;; init-irc.el --- init for irc -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Internet Relay Chat in Emacs.
;;

;;; Code:

(use-package erc
  :defer t
  :init
  ;; Prerequisite: Configure this to your IRC nickname
  (defcustom my-irc-nick ""
    "The nickname used to login into ERC"
    :type 'string)

  (defvar my-fav-irc '(
                       "irc.libera.chat"
                       ;; "irc.gitter.im"
                       ;; ;; so sad freenode is not libre anymore
                       ;; "irc.freenode.net"
                       )
    "The list of IRC servers that want to connect to with `my-start-irc.'")

  (defvar bye-irc-message "孤帆远影碧空尽，唯见长江天际流"
    "Message string to be sent while quitting IRC.")

  (defun my-connect-irc (server)
    "Connects securely to IRC SERVER over TLS at port 6697."
    (erc-tls :server server
             :port 6697
             :nick my-irc-nick))

  (defun my-start-irc ()
    "Connect to IRC?"
    (interactive)
    (when (y-or-n-p "Do you want to start IRC? ")
      (mapcar #'my-connect-irc my-fav-irc)))

  :config
  (require 'erc-log)
  (require 'erc-notify)
  (require 'erc-spelling)
  (require 'erc-autoaway)

  (setq erc-autojoin-channels-alist '((
                                       "libera.chat"
                                       "#nixos"
                                       "#emacs"
                                       )))

  ;; Interpret mIRC-style color commands in IRC chats
  (setq erc-interpret-mirc-color t)

  ;; The following are commented out by default, but users of other
  ;; non-Emacs IRC clients might find them useful.
  ;; Kill buffers for channels after /part
  (setq erc-kill-buffer-on-part t)
  ;; Kill buffers for private queries after quitting the server
  (setq erc-kill-queries-on-quit t)
  ;; Kill buffers for server messages after quitting the server
  (setq erc-kill-server-buffer-on-quit t)
  ;; open query buffers in the current window
  (setq erc-query-display 'buffer)

  ;; exclude boring stuff from tracking
  (erc-track-mode +1)
  (setq erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                                  "324" "329" "332" "333" "353" "477"))

  ;; logging
  (unless (file-exists-p erc-log-channels-directory)
    (mkdir erc-log-channels-directory t))

  ;; truncate long irc buffers
  (erc-truncate-mode +1)

  ;; autoaway setup
  (setq erc-auto-discard-away t)
  (setq erc-autoaway-idle-seconds 600)
  (setq erc-autoaway-use-emacs-idle t)

  (defun my-filter-server-buffers ()
    "Filter all irc servers."
    (delq nil
          (mapcar
           (lambda (x) (and (erc-server-buffer-p x) x))
           (buffer-list))))

  (defun my-stop-irc ()
    "Disconnect from all irc servers."
    (interactive)
    (dolist (buffer (my-filter-server-buffers))
      (message "Server buffer: %s" (buffer-name buffer))
      (with-current-buffer buffer
        (erc-quit-server bye-irc-message))))

  (defun my-erc-browse-last-url ()
    "Search backwards through an ERC buffer, looking for a URL.
When a URL is found, it prompts you to open it."
    (interactive)
    (save-excursion
      (let ((ffap-url-regexp "\\(https?://\\)."))
        (ffap-next-url t t))))

  (defun my-erc-count-users ()
    "Display the number of users and ops connected on the current channel."
    (interactive)
    (if (get-buffer "irc.freenode.net:6667")
        (let ((channel (erc-default-target)))
          (if (and channel (erc-channel-p channel))
              (let ((hash-table (with-current-buffer (erc-server-buffer)
                                  erc-server-users))
                    (users 0)
                    (ops 0))
                (maphash (lambda (k v)
                           (when (member (current-buffer)
                                         (erc-server-user-buffers v))
                             (cl-incf users))
                           (when (erc-channel-user-op-p k)
                             (cl-incf ops)))
                         hash-table)
                (message "%d users (%s ops) are online on %s"
                         users ops channel))
            (user-error "The current buffer is not a channel")))
      (user-error "You must first be connected on IRC")))

  (defun my-erc-get-ops ()
    "Display the names of ops users on the current channel."
    (interactive)
    (if (get-buffer "irc.freenode.net:6667")
        (let ((channel (erc-default-target)))
          (if (and channel (erc-channel-p channel))
              (let (ops)
                (maphash (lambda (nick cdata)
                           (when (and (cdr cdata)
                                      (erc-channel-user-op (cdr cdata)))
                             (setq ops (cons nick ops))))
                         erc-channel-users)
                (if ops
                    (message "The online ops users are: %s"
                             (mapconcat #'identity ops " "))
                  (message "There are no ops users online on %s" channel)))
            (user-error "The current buffer is not a channel")))
      (user-error "You must first be connected on IRC")))

  (defun my-erc-notify (nickname message)
    "Display a notification MESSAGE for ERC according to NICKNAME."
    (let* ((channel (buffer-name))
           (nick (erc-hl-nicks-trim-irc-nick nickname))
           (title (if (string-match-p (concat "^" nickname) channel)
                      nick
                    (concat nick " (" channel ")")))
           (msg (s-trim (s-collapse-whitespace message))))
      (alert (concat nick ": " msg) :title title)))
  (add-hook 'ercn-notify-hook #'my-erc-notify)

  ;; ----
  ;; misc
  ;; ----

  ;; utf-8 always and forever
  (setq erc-server-coding-system '(utf-8 . utf-8)))

(provide 'init-irc)

;;; init-irc.el ends here
