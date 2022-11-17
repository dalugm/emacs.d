;;; init-python.el --- Python programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Python configuration.
;;

;;; Code:

(use-package python
  :mode ("\\.\\(c\\|i\\|r\\)?py\\'" . python-mode)
  :custom
  (python-indent-guess-indent-offset nil)
  (python-indent-offset 4))

;; To switch kernels first run
;;
;; ,---
;; | pip install ipykernel
;; `---
;;
;; and then
;;
;; ,---
;; | python -m ipykernel install --user --name=<PYVENV NAME>
;; `---
;;
;; where <PYVENV_NAME> is a name you give to this ipython kernel
;;
;; To manage jupyter kernels, run
;;
;; ,---
;; | jupyter kernelspec
;; `---
;;
(use-package ein
  :disabled
  :defer t
  :config
  ;; output the images directly in the Emacs buffer
  (when (display-graphic-p)
    (setq ein:output-area-inlined-images t)))

(provide 'init-python)

;;; init-python.el ends here
