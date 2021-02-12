;;; init-prog.el --- program configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Basic configuration for prog-mode.
;;

;;; Code:

;; ---------------------------------------------------------
;; compile
;; ---------------------------------------------------------
;; Compilation from Emacs
(defun my//colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(require 'compile)
;; Just save before compiling
(setq compilation-ask-about-save nil)
;; Just kill old compile processes before starting the new one
(setq compilation-always-kill t)
;; Automatically scroll to first error
(setq compilation-scroll-output 'first-error)

;; Colorize output of Compilation Mode
;; https://stackoverflow.com/a/3072831/355252
(require 'ansi-color)
(add-hook 'compilation-filter-hook #'my//colorize-compilation-buffer)

;; https://ebzzry.io/en/emacs-pairs/
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :bind (:map smartparens-mode-map
          ("C-M-a" . sp-beginning-of-sexp)
          ("C-M-e" . sp-end-of-sexp)
          ("C-M-d" . sp-down-sexp)
          ("C-M-u" . sp-backward-up-sexp)
          ("C-M-f" . sp-forward-sexp)
          ("C-M-b" . sp-backward-sexp)
          ("C-M-n" . sp-next-sexp)
          ("C-M-p" . sp-previous-sexp)
          ("C-M-k" . sp-kill-sexp)
          ("C-M-t" . sp-transpose-sexp)
          ("M-("   . sp-backward-slurp-sexp)
          ("M-s [" . sp-backward-slurp-sexp)
          ("M-)"   . sp-forward-slurp-sexp)
          ("M-s ]" . sp-forward-slurp-sexp)
          ("M-_"   . sp-backward-barf-sexp)
          ("M-s {" . sp-backward-barf-sexp)
          ("M-+"   . sp-forward-barf-sexp)
          ("M-s }" . sp-forward-barf-sexp)
          ("M-s u" . sp-unwrap-sexp)
          ("M-R"   . sp-raise-sexp)
          ("M-s r" . sp-raise-sexp)
          ("M-S"   . sp-split-sexp)
          ("M-s s" . sp-split-sexp)
          ("C-c ("  . my/wrap-with-parens)
          ("C-c ["  . my/wrap-with-brackets)
          ("C-c {"  . my/wrap-with-braces)
          ("C-c '"  . my/wrap-with-single-quotes)
          ("C-c \"" . my/wrap-with-double-quotes)
          ("C-c _"  . my/wrap-with-underscores)
          ("C-c `"  . my/wrap-with-back-quotes))
  :config
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  (show-smartparens-global-mode)

  ;; Overlays are too distracting and not terribly helpful. show-paren does
  ;; this for us already (and is faster), so...
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)

  (with-eval-after-load 'evil
    ;; But if someone does want overlays enabled, evil users will be stricken
    ;; with an off-by-one issue where smartparens assumes you're outside the
    ;; pair when you're really at the last character in insert mode. We must
    ;; correct this vile injustice.
    (setq sp-show-pair-from-inside t)
    ;; ...and stay highlighted until we've truly escaped the pair!
    (setq sp-cancel-autoskip-on-backward-movement nil))

  ;; ;; disable add parenthesis inside string
  ;; ;; author recommends to use `C-q' to insert instead
  ;; ;; https://github.com/Fuco1/smartparens/issues/683
  ;; (sp-pair "(" nil :unless '(sp-in-string-p))

  ;; config for `sp-wrap'
  (defmacro my|def-pairs (pairs)
    "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (my|def-pairs ((paren . \"(\")
                 (bracket . \"[\")))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
    `(progn
       ,@(cl-loop for (key . val) in pairs
           collect
           `(defun ,(read (concat
                            "my/wrap-with-"
                            (prin1-to-string key)
                            "s"))
              (&optional arg)
              (interactive "p")
              (sp-wrap-with-pair ,val)))))

  (my|def-pairs ((paren . "(")
                 (bracket . "[")
                 (brace . "{")
                 (angle . "<")
                 (single-quote . "'")
                 (double-quote . "\"")
                 (back-quote . "`"))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-prog)

;;; init-prog.el ends here
