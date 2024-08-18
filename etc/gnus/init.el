;;; init.el --- gnus configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Manage emails in Emacs.
;;

;;; Code:

;; Send mail through SMTP.
(setopt send-mail-function #'smtpmail-send-it)

(setopt user-mail-address "mou.tong@qq.com"
        user-full-name "Mou Tong")

(setopt smtpmail-smtp-user "mou.tong@qq.com"
        smtpmail-smtp-server "smtp.qq.com"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'ssl)

(setq gnus-select-method
      '(nnimap "qq.com"
               (nnimap-address "imap.qq.com")
               (nnimap-inbox "INBOX")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

;; Fetch only part of the article if we can.
(setopt gnus-read-active-file 'some)
(setopt nnmail-expiry-wait 'never)
(setopt nnmail-expiry-target "Deleted Messages")

;; ;; Debug.
;; (setopt smtpmail-debug-info t)
;; (setopt smtpmail-debug-verb t)

;;; init.el ends here
