;;; init-sexp.el --- S-expression -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuration for dealing with S-expressions.
;;

;;; Code:

;; https://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html
(defun my-endless-sharp ()
  "Insert #' unless in a string or comment."
  (interactive)
  (call-interactively #'self-insert-command)
  (let ((ppss (syntax-ppss)))
    (unless (or (elt ppss 3)
                (elt ppss 4)
                (eq (char-after) ?'))
      (insert "'"))))

(define-key emacs-lisp-mode-map "#" #'my-endless-sharp)

(defun my-eval-last-sexp ()
  "Evaluate the last symbolic expression at the point.
With nil `C-u' prefix, insert output below following an arrow.
With one `C-u' prefix, insert output in current position.
With two `C-u' prefix, insert output in current position and delete sexp."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (save-excursion
      (cond
       ((equal current-prefix-arg nil) ; no prefix
        (newline-and-indent)
        (insert (format "%s%S" ";; => " value)))
       ((equal current-prefix-arg '(4)) ; one prefix
        (newline-and-indent)
        (insert (format "%S" value)))
       ((equal current-prefix-arg '(16)) ; two prefix
        (backward-kill-sexp)
        (insert (format "%S" value)))))))

(dolist (map (list emacs-lisp-mode-map
                   lisp-interaction-mode-map))
  (define-key map (kbd "C-c C-e") #'my-eval-last-sexp))

;; https://www.travishinkelman.com/posts/getting-started-with-chez-scheme-and-emacs/
(use-package geiser
  :commands geiser
  :config
  (setq geiser-mode-smart-tab-p t))

(provide 'init-sexp)

;;; init-sexp.el ends here
