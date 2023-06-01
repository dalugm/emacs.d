;;; init-chinese.el --- Chinese integration for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Chinese integration.
;;

;;; Code:

(use-package pyim
  :bind (("C-\\" . toggle-input-method)
         ("C-c e c" . pyim-convert-string-at-point)
         (:map pyim-mode-map
               ("," . pyim-page-previous-page)
               ("." . pyim-page-next-page)))
  :custom
  (default-input-method "pyim")
  (pyim-default-scheme 'quanpin)
  ;; Compatible with terminal.
  (pyim-page-tooltip 'minibuffer)
  (pyim-page-style 'two-lines)
  (pyim-page-length 9)
  :config
  (setq pyim-fuzzy-pinyin-alist
        '(("en" "eng")
          ("in" "ing")))

  ;; Use memory efficient pyim engine.
  (require 'pyim-dregcache)
  (setq pyim-dcache-backend 'pyim-dregcache)

  ;; Change input method automatically.
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  (defvar my-pyim-directory (expand-file-name "pyim/" my-cache-d)
    "The directory containing pyim related files.")

  (unless (file-directory-p my-pyim-directory) (mkdir my-pyim-directory))

  ;; pyim-bigdict is recommended (20M).
  ;; There are too many useless words in pyim-greatdict
  ;; which also slows down pyim performance.
  ;; curl -L https://tumashu.github.io/pyim-bigdict/pyim-bigdict.pyim.gz | zcat > path/to/my-pyim-directory/pyim-bigdict.pyim
  ;; load all "*.pyim" under `my-pyim-directory'.
  (let ((files (directory-files-recursively
                my-pyim-directory
                "\\.pyim\\'"))
        disable-basedict)
    (when (and files (> (length files) 0))
      (setq pyim-dicts
            (mapcar
             (lambda (f)
               (list :name (file-name-base f) :file f))
             files))
      ;; Disable basedict if a local dict is used.
      (setq disable-basedict t))
    (unless disable-basedict (pyim-basedict-enable))))

(provide 'init-chinese)

;;; init-chinese.el ends here
