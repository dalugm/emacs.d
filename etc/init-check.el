;;; init-check.el --- checker for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Syntax and spelling check.
;;

;;; Code:

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-global-modes
        '(not
           text-mode
           outline-mode
           fundamental-mode
           lisp-interaction-mode
           org-mode
           diff-mode
           shell-mode
           eshell-mode
           term-mode
           vterm-mode
           ))
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-indication-mode (if (display-graphic-p)
                                     'left-fringe
                                   'left-margin))
  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package wucuo
  :hook ((prog-mode . wucuo-start)
         (text-mode . wucuo-start))
  :config
  ;; Aspell Setup (recommended):
  ;; It's easy to set up aspell. So the details are skipped.
  ;;
  ;; Hunspell Setup:
  ;; 1. Install hunspell from https://hunspell.github.io/
  ;;
  ;; 2. Download openoffice dictionary extension from
  ;; https://extensions.openoffice.org/en/project/english-dictionaries-apache-openoffice
  ;;
  ;; 3. Say `dict-en.oxt' is downloaded. Rename it to `dict-en.zip' and unzip
  ;; the contents to a temporary folder. Got "en_US.dic" and "en_US.aff" in
  ;; that folder.
  ;;
  ;; 4. Hunspell's option "-d en_US" means finding dictionary "en_US"
  ;; Set `ispell-local-dictionary-alist' to set that option of hunspell
  ;;
  ;; 5. Copy "en_US.dic" and "en_US.aff" from that temporary folder to
  ;; the place for dictionary files. I use "/usr/local/share/hunspell/".
  ;;
  ;; 6. Add that folder to shell environment variable "DICPATH"
  ;;
  ;; 7. Restart emacs so when hunspell is run by ispell/flyspell to make
  ;; DICPATH take effect
  ;;
  ;; hunspell searches a dictionary named "en_US" in the path specified by
  ;; `$DICPATH' by default.
  ;; EXAMPLE:
  ;; (setq ispell-program-name "hunspell")
  ;; ;; below two lines reset the the hunspell to it STOPS querying locale!
  ;; (setq ispell-local-dictionary "myhunspell") ; "myhunspell" is key to lookup in `ispell-local-dictionary-alist`
  ;; (setq ispell-local-dictionary-alist
  ;;       '(("myhunspell" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US" "zh_CN") nil utf-8)))

  (setq ispell-dictionary "en_US")
  (setq ispell-program-name "aspell")

  (setq wucuo-spell-check-buffer-predicate
        (lambda ()
          (not (memq major-mode
                     '(dired-mode
                       log-edit-mode
                       compilation-mode
                       help-mode
                       profiler-report-mode
                       speedbar-mode
                       calc-mode
                       Info-mode))))))

(provide 'init-check)

;;; init-check.el ends here
