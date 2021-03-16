;;; init-sexp.el --- S-expression -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuration for dealing with S-expressions.
;;

;;; Code:

;; https://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html
(defun my//endless-sharp ()
  "Insert #' unless in a string or comment."
  (interactive)
  (call-interactively #'self-insert-command)
  (let ((ppss (syntax-ppss)))
    (unless (or (elt ppss 3)
                (elt ppss 4)
                (eq (char-after) ?'))
      (insert "'"))))

(define-key emacs-lisp-mode-map "#" #'my//endless-sharp)

;; https://www.travishinkelman.com/posts/getting-started-with-chez-scheme-and-emacs/
(use-package geiser
  :commands geiser
  :config
  ;; geiser replies on a REPL to provide autodoc and completion
  (setq geiser-mode-start-repl-p t)
  (setq geiser-mode-smart-tab-p t))

(use-package slime
  :commands slime
  :config
  (setq inferior-lisp-program "sbcl"))

(provide 'init-sexp)

;;; init-sexp.el ends here
