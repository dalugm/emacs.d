;;; init.el --- gnus configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Manage emails in Emacs.
;;

;;; Code:

;; Send mail through SMTP.
(require 'smtpmail)
(setq message-send-mail-function #'smtpmail-send-it)

;; Outlook.
(setq smtpmail-smtp-user "mou.tong@outlook.com"
      smtpmail-smtp-server "smtp.office365.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls)

(with-eval-after-load 'gnus
  ;; Fetch only part of the article if we can.
  (setq gnus-read-active-file 'some)
  (setq nnmail-expiry-wait 'never)
  (setq nnmail-expiry-target "Deleted Messages"))

;; ;; Debug.
;; (setq smtpmail-debug-info t)
;; (setq smtpmail-debug-verb t)

;;; init.el ends here
