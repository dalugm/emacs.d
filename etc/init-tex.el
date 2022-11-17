;;; init-tex.el --- tex configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; TeX configuration.
;;

;;; Code:

(with-eval-after-load 'tex-mode
  (setq tex-command "xelatex")
  (add-to-list 'tex-compile-commands '("xelatex %f" t "%r.pdf")))

(provide 'init-tex)

;;; init-tex.el ends here
