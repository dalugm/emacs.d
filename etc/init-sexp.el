;;; init-sexp.el --- S-expression -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuration for dealing with S-expressions.
;;

;;; Code:

;; https://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html
(defun my/endless-sharp ()
  "Insert #' unless in a string or comment."
  (interactive)
  (call-interactively #'self-insert-command)
  (let ((ppss (syntax-ppss)))
    (unless (or (elt ppss 3)
                (elt ppss 4)
                (eq (char-after) ?'))
      (insert "'"))))
(define-key emacs-lisp-mode-map "#" #'my/endless-sharp)

;; https://www.travishinkelman.com/posts/getting-started-with-chez-scheme-and-emacs/
(use-package geiser
  :commands run-geiser
  :init
  ;; binary config
  (let ((my--mit-binary (cond
                          (sys/macp "/usr/local/bin/mit-scheme")
                          (sys/linuxp "/home/root/bin/mit-scheme")
                          (sys/winp "C:/Program Files/Mit Scheme/bin/scheme"))))
    (setq geiser-mit-binary my--mit-binary))
  (let ((my--guile-binary (cond
                            (sys/macp "/usr/local/bin/guile")
                            (sys/linuxp "/home/root/bin/guile")
                            (sys/winp "C:/Program Files/Guile Scheme/bin/scheme"))))
    (setq geiser-guile-binary my--guile-binary))
  (let ((my--chez-binary (cond
                           (sys/macp "/usr/local/bin/chez")
                           (sys/linuxp "/home/root/bin/chez")
                           (sys/winp "C:/Program Files/Chez Scheme/bin/scheme"))))
    (setq geiser-chez-binary my--chez-binary))
  :config
  (setq geiser-active-implementations '(mit guile chez))
  (setq geiser-mode-smart-tab-p t))

(use-package slime
  :bind ("C-c t s" . slime)
  :config (setq inferior-lisp-program "sbcl"))

(provide 'init-sexp)

;;; init-sexp.el ends here
