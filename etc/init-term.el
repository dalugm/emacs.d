;;; init-term.el --- terminal configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuration about term/shell in Emacs.
;;
;; Using term is not recommended,
;; because normal EMACS keys won't work.
;;

;;; Code:

(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook (lambda ()
                                ;; alias
                                (eshell/alias "f" "find-file $1")
                                (eshell/alias "fo" "find-file-other-window $1")
                                (eshell/alias "d" "dired $1")
                                (eshell/alias "l" "ls -ahlG"))))

(provide 'init-term)

;;; init-term.el ends here
