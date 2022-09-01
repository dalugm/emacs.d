;;; init-chinese.el --- Chinese integration for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Chinese integration.
;;

;;; Code:

;; ---------------------------------------------------------
;; Emacs IME
;; ---------------------------------------------------------

;; ----- pyim ----------------------------------------------
(use-package pyim
  :unless (featurep 'rime)
  :bind (("C-\\" . toggle-input-method)
         (:map pyim-mode-map
          ("," . pyim-page-previous-page)
          ("." . pyim-page-next-page)))
  :custom (default-input-method "pyim")
  :config
  (setq pyim-default-scheme 'quanpin)

  ;; compatible with terminal
  (setq pyim-page-tooltip 'minibuffer)
  (setq pyim-page-style 'two-lines)
  (setq pyim-page-length 9)
  (setq pyim-fuzzy-pinyin-alist
        '(("en" "eng")
          ("in" "ing")))

  ;; ;; Rime config
  ;; (liberime-start
  ;;   (if sys/macp
  ;;       "/Library/Input Methods/Squirrel.app/Contents/SharedSupport"
  ;;     "/usr/share/rime-data")
  ;;   (expand-file-name "rime/" my-cache-d))
  ;; (liberime-select-schema "luna_pinyin")

  ;; use memory efficient pyim engine
  (setq pyim-dcache-backend 'pyim-dregcache)

  ;; cooperate with `pyim-probe-dynamic-english'
  (global-set-key (kbd "M-j") #'pyim-convert-string-at-point)

  ;; change input method automatically
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; -------------------------------------------------------
  ;; dict
  ;; -------------------------------------------------------
  (defvar my-pyim-directory (expand-file-name "pyim/" my-cache-d)
    "The directory containing pyim dictionaries.")

  ;; pyim-bigdict is recommended (20M).
  ;; There are too many useless words in pyim-greatdict
  ;; which also slows down pyim performance.
  ;; `curl -L https://tumashu.github.io/pyim-bigdict/pyim-bigdict.pyim.gz | zcat > path/to/pyim-bigdict.pyim`
  ;; automatically load all "*.pyim" under `my-cache-d'
  ;; `directory-files-recursively' requires Emacs 25
  (let ((files (and (file-exists-p my-pyim-directory)
                    (directory-files-recursively my-pyim-directory "\.pyim$")))
        disable-basedict)
    (when (and files (> (length files) 0))
      (setq pyim-dicts
            (mapcar (lambda (f)
                      (list :name (file-name-base f) :file f))
              files))
      ;; disable basedict if bigdict or greatdict is used
      (dolist (f files)
        (when (or (string= "pyim-bigdict" (file-name-base f))
                  (string= "pyim-greatdict" (file-name-base f)))
          (setq disable-basedict t))))
    (unless disable-basedict (pyim-basedict-enable))))

(use-package bing-dict
  :bind ("C-c s b" . bing-dict-brief))

(use-package avy-zh
  :after avy
  :config (global-avy-zh-mode))

(provide 'init-chinese)

;;; init-chinese.el ends here
