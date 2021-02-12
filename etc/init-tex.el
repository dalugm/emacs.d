;;; init-tex.el --- tex configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; TeX configuration.
;;

;;; Code:

(with-eval-after-load 'tex-mode
  (setq tex-command "xelatex")
  (add-to-list 'tex-compile-commands '("xelatex %f" t "%r.pdf")))

(with-eval-after-load 'auctex
  ;; Use `xetex' engine for better TeX compilation for Chinese.
  ;; `TeX-engine-alist', `TeX-engine-in-engine-alist'
  (setq TeX-engine 'xetex)
  (setq TeX-command "xelatex")
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t)))

(provide 'init-tex)

;;; init-tex.el ends here
