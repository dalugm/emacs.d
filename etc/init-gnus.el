;;; init-gnus.el --- email configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Read emails in Emacs.
;;

;;; Code:

;; Patch security vulnerability in Emacs versions older than 25.3
;; https://bugs.debian.org/766397
;; https://www.cvedetails.com/cve/CVE-2017-14482/
(when (version< emacs-version "25.3")
  (with-eval-after-load 'enriched
    (defun enriched-decode-display-prop (start end &optional _param)
      (list start end))))

;; send mail through SMTP
(require 'smtpmail)
(setq message-send-mail-function #'smtpmail-send-it)

;; Outlook
(setq smtpmail-smtp-user "mou.tong@outlook.com"
      smtpmail-smtp-server "smtp.office365.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls)

(with-eval-after-load 'gnus

  ;; Fetch only part of the article if we can.
  (setq gnus-read-active-file 'some)

  ;; Delete mail
  (setq nnmail-expiry-wait 'never)
  (setq nnmail-expiry-target "Deleted Messages"))

;; ;; DeBUG
;; (setq smtpmail-debug-info t)
;; (setq smtpmail-debug-verb t)

(provide 'init-gnus)

;;; init-gnus.el ends here
