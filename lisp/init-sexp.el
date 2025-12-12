;;; init-sexp.el --- S-expression -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuration for dealing with S-expressions.
;;

;;; Code:

(defun my-endless-sharp ()
  "Insert #\\=' unless in a string or comment.

URL `https://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html'."
  (interactive)
  (call-interactively #'self-insert-command)
  (let ((ppss (syntax-ppss)))
    (unless (or (elt ppss 3)
                (elt ppss 4)
                (eq (char-after) ?\'))
      (insert "'"))))

(keymap-set emacs-lisp-mode-map "#" #'my-endless-sharp)

(defun my-eval-print-last-sexp (&optional arg)
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
            ;; If return value is a multiline string.
            (insert (format
                     ";; =>\n;; %S"
                     (replace-regexp-in-string "\n" "\n;; " value)))
          (insert (format "%s%S" ";; => " value))))
       ((equal arg '(4))
        (backward-kill-sexp)
        (insert (format "%S" value)))))))

(dolist (map (list emacs-lisp-mode-map
                   lisp-interaction-mode-map))
  (keymap-set map "C-c C-p" #'my-eval-print-last-sexp))

(use-package sly
  :config
  (define-advice sly-mrepl (:override (&rest _) last)
    "Switch to the last Lisp/Sly-Mrepl buffer."
    (interactive)
    (if (derived-mode-p 'sly-mrepl-mode)
        (if-let* ((buf (seq-find (lambda (b)
                                   (with-current-buffer b
                                     (derived-mode-p 'lisp-mode)))
                                 (buffer-list))))
            (if-let* ((win (get-buffer-window buf)))
                (select-window win)
              (pop-to-buffer buf))
          (user-error "No Lisp buffer found"))
      (if-let* ((buf (sly-mrepl--find-create (sly-current-connection))))
          (if-let* ((win (get-buffer-window buf)))
              (select-window win)
            (pop-to-buffer buf))
        (user-error "No Sly-Mrepl buffer found"))))
  :bind ((:map sly-mode-map
               ("C-c C-a C-c" . sly-asdf-compile-system)
               ("C-c C-a C-l" . sly-asdf-load-system)
               ("C-c C-a C-r" . sly-asdf-reload-system)
               ("C-c C-a C-t" . sly-asdf-test-system)
               ("C-c C-x C-c" . sly-connect)
               ("C-c C-x C-q" . sly-disconnect)
               ("C-c C-x C-j" . sly))
         (:map sly-doc-map
               ("C-l" . sly-documentation)))
  :custom (inferior-lisp-program "sbcl"))

(use-package sly-asdf :after sly)
(use-package sly-quicklisp :after sly)

(use-package racket-mode
  :bind (:map racket-mode-map
              ("C-c C-x C-j" . racket-run)
              ("C-c C-x C-x" . racket-xp-mode)
              ("C-c C-x C-e" . racket-eval-last-sexp)))

(use-package clojure-mode
  :mode
  ("\\.\\(cljd?\\|edn\\)\\'" . clojure-mode)
  ("\\.cljs\\'" . clojurescript-mode))

(use-package clojure-ts-mode
  :when (treesit-available-p)
  :mode
  ("\\.\\(clj\\|edn\\)\\'" . clojure-ts-mode)
  ("\\.cljs\\'" . clojure-ts-clojurescript-mode)
  ("\\.cljd\\'" . clojure-ts-clojuredart-mode))

(use-package cider
  :bind ((:map cider-mode-map
               ("C-c C-i C-i" . cider-inspect)
               ("C-c C-i C-r" . cider-inspect-last-result)
               ("C-c C-i C-s" . cider-inspect-last-sexp))
         (:map cider-start-map
               ("r" . cider-restart)
               ("C-r" . cider-restart)))
  :custom
  ;; Require Java >= 21.
  (cider-enable-nrepl-jvmti-agent t)
  :config
  ;; https://github.com/clojure-emacs/cider/issues/3588
  (when (and (string= "powershell" cider-clojure-cli-command)
             (executable-find "pwsh"))
    (setopt cider-clojure-cli-command "pwsh")))

(use-package fennel-mode
  :mode ("\\.fnl\\'" . fennel-mode))

(provide 'init-sexp)
;;; init-sexp.el ends here
