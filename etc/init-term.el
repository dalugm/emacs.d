;;; init-term.el --- terminal configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuration about term/shell in Emacs
;;
;; using term is not recommended
;; because normal EMACS keys won't work.
;;

;;; Code:

;; use minimum eshell prompt
(with-eval-after-load 'eshell
  (setq eshell-prompt-function
    (lambda ()
      (concat (getenv "USER") " $ "))))

(provide 'init-term)

;;; init-term.el ends here
