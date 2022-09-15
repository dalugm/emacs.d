;;; init-python.el --- Python programming in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Python configuration.
;;

;;; Code:

(defun my--python-mode-hook-setup ()
  "Default configuration for python."
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  (setq python-indent-offset 4)
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH")))

(add-hook 'python-mode-hook #'my--python-mode-hook-setup)

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
