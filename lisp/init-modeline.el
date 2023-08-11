;;; init-modeline.el --- config for modeline -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Show necessary messages only on modeline.
;;

;;; Code:

;; Use `setq-default' to set it for all modes.
;; https://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Format.html#Mode-Line-Format

;;;; Segments.

;; NOTE: Unless the symbol has a non-nil `risky-local-variable'
;; property, all properties in any strings, as well as all `:eval' and
;; `:propertize' forms in the value, are ignored.

(defvar my--mode-line-buffer-identification
  '(:propertize " %b ")
  "More informative than `buffer-id'.")
(put 'my--mode-line-buffer-identification 'risky-local-variable t)

(defvar my--mode-line-position
  '(" %l:%C ")
  "Display the position in the buffer.")
(put 'my--mode-line-position 'risky-local-variable t)

(defvar my--mode-line-file-info
  '("("
    (:propertize "%p" 'face nil)
    ;; Judge between local and remote.
    (:propertize "%@" 'face nil)
    (:propertize "%I" 'face nil)
    ")")
  "Display file info.")
(put 'my--mode-line-file-info 'risky-local-variable t)

(defvar my--mode-line-modes
  '(""
    "%["
    (:propertize mode-name)
    mode-line-process
    "%n"
    "%]")
  "Show major mode only.")
(put 'my--mode-line-modes 'risky-local-variable t)

(defvar my--mode-line-encoding
  '(:eval
    (let ((sys (coding-system-plist buffer-file-coding-system)))
      (if (memq (plist-get sys :category)
                '(coding-category-undecided coding-category-utf-8))
          "UTF-8"
        (upcase (symbol-name (plist-get sys :name))))))
  "Display file encoding.")
(put 'my--mode-line-encoding 'risky-local-variable t)

;;;; Setup.

(defvar-local my-mode-line-format-left nil
  "Mode-line left component.")
(put 'my-mode-line-format-left 'risky-local-variable t)

(defvar-local my-mode-line-format-right nil
  "Mode-line right component.")
(put 'my-mode-line-format-right 'risky-local-variable t)

(setq-default my-mode-line-format-left
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                my--mode-line-buffer-identification
                my--mode-line-position
                mode-line-frame-identification
                my--mode-line-file-info
                " "
                evil-mode-line-tag))

(setq-default my-mode-line-format-right
              '(""
                mode-line-misc-info
                " "
                my--mode-line-modes
                (vc-mode vc-mode)
                " "
                my--mode-line-encoding
                " "))

(defvar my-mode-line-format
  '(""
    my-mode-line-format-left
    (:eval
     (propertize
      " "
      'display
      `((space :align-to (- (+ right right-fringe right-margin)
                            ,(string-width
                              (format-mode-line
                               '("" my-mode-line-format-right))))))))
    my-mode-line-format-right)
  "My customized mode-line.")

(setq-default mode-line-format my-mode-line-format)

;;;; Misc

;; ;; If you want to customize time format, read document of
;; ;; `format-time-string' and customize `display-time-format'.
;; (setq display-time-format "%a %b %e")

;; Unify the eol mnemonics for all systems.
(setq eol-mnemonic-unix ":")
(setq eol-mnemonic-mac "/")
(setq eol-mnemonic-dos "\\")

(setq system-time-locale "C")
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
;; Do NOT display the load average.
(setq display-time-default-load-average nil)
;; Show date in mode-line.
(display-time)

;; Make the position number update correctly in all cases.
(line-number-mode +1)
(column-number-mode +1)

;; Human readable representation of file size in mode-line.
(size-indication-mode +1)

(provide 'init-modeline)

;;; init-modeline.el ends here
