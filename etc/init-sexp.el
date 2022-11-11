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

(defun my-eval-last-sexp (&optional arg)
  "Evaluate sexp before point, insert output below following an arrow.
With a `\\[universal-argument]' prefix argument ARG, delete the
sexp before point and insert output into current position."
  (interactive "P")
  (let ((value (eval (elisp--preceding-sexp))))
    (save-excursion
      (cond
       ((not arg)
        (newline-and-indent)
        (if (and (stringp value) (string-match-p "\n" value))
            ;; if return value is a multiline string
            (insert (format
                     ";; =>\n;; %S"
                     (replace-regexp-in-string "\n" "\n;; " value)))
          (insert (format "%s%S" ";; => " value))))
       ((equal arg '(4))
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
