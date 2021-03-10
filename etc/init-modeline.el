;;; init-modeline.el --- config for modeline -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Show necessary message on modeline.
;;

;;; Code:

;; use `setq-default' to set it for all modes
;; https://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Format.html#Mode-Line-Format

;; current structure use `doom-emacs' for reference
;; divide mode line segments
;; https://github.com/hlissner/doom-emacs-private/blob/master/lisp/modeline.el

;;
;;; Segments
;;

;; NOTE unless the symbol has a non-nil
;; `risky-local-variable' property, all properties in any strings, as
;; well as all :eval and :propertize forms in the value, are ignored.

;; ----- `my--mode-line-modes' -----------------------------
(defvar my--mode-line-modes
  '(""
     "%["
     (:propertize mode-name
       face bold
       mouse-face mode-line-highlight)
     mode-line-process
     "%n"
     "%]"
     " ")
  "Remove minor modes.")
(put 'my--mode-line-modes 'risky-local-variable t)

;; ----- `my--mode-line-buffer-identification' -------------
;; more informative than `buffer-id'
(defvar my--mode-line-buffer-identification
  '((:eval
      (propertize " %b "
        ;; was this buffer modified since the last save?
        'face (cond
                ((buffer-modified-p)
                  '(error mode-line-buffer-id))
                (t
                  'nil))
        'help-echo buffer-file-name))
     (:eval (when (buffer-modified-p)
              (propertize " MOD "
                'face nil
                'help-echo "Buffer has been modified")))
     (buffer-read-only (:propertize " RO "
                         face warning
                         help-echo "Buffer is read-only")))
  "More informative than `buffer-id'.")
(put 'my--mode-line-buffer-identification 'risky-local-variable t)

;; ----- `my--mode-line-position' --------------------------
(defvar my--mode-line-position
  '(" %l:%C ")
  "Display the position in the buffer.")
(put 'my--mode-line-position 'risky-local-variable t)

;; ----- `my--mode-line-file' ------------------------------
(defvar my--mode-line-file
  '((:eval
      (concat
        "("
        (propertize "%p" 'face nil)
        ;; judge between local and remote
        (propertize "%@" 'face nil)
        (propertize "%I" 'face nil)
        ")"))))
(put 'my--mode-line-file 'risky-local-variable t)

;; ----- `my--mode-line-time' ------------------------------
(defvar my--mode-line-time
  '((:eval
      (propertize " %M " 'face nil)))
  "Display time on mode line.")
(put 'my--mode-line-time 'risky-local-variable t)

;; ----- `my--mode-line-encoding' --------------------------
(defvar my--mode-line-encoding
  '(:eval
     (concat (let ((eol (coding-system-eol-type buffer-file-coding-system)))
               (pcase eol
                 (0 " LF ")
                 (1 " CRLF ")
                 (2 " CR ")
                 (_ " ")))
       (let ((sys (coding-system-plist buffer-file-coding-system)))
         (if (memq (plist-get sys :category)
               '(coding-category-undecided coding-category-utf-8))
             "UTF-8"
           (upcase (symbol-name (plist-get sys :name)))))
       "  ")))
(put 'my--mode-line-encoding 'risky-local-variable t)

;;
;;; Setup
;;

(defvar-local mode-line-format-left nil)
(put 'mode-line-format-left 'risky-local-variable t)

(defvar-local mode-line-format-right nil)
(put 'mode-line-format-right 'risky-local-variable t)

(setq-default mode-line-format-left
  '(""
     my--mode-line-buffer-identification
     my--mode-line-position
     " "
     my--mode-line-file
     " "
     evil-mode-line-tag
     my--mode-line-time))

(setq-default mode-line-format-right
  `(""
     my--mode-line-misc-info
     my--mode-line-modes
     (vc-mode vc-mode)
     my--mode-line-encoding))

(defvar my-mode-line-format
  '(""
     mode-line-format-left
     (:eval
       (propertize " "
         'display
         `((space :align-to (- (+ right right-fringe right-margin)
                              ,(string-width
                                 (format-mode-line
                                   '("" mode-line-format-right))))))))
     mode-line-format-right)
  "My customized mode-line.")

(setq-default mode-line-format my-mode-line-format)

;;
;;; Misc
;;

;; ----- format ---------------------------------------
;; If you want to customize time format, read document of `format-time-string'
;; and customize `display-time-format'.
;; (setq display-time-format "%a %b %e")

;; see `info'
(setq system-time-locale "C")
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
;; do NOT display the load average
(setq display-time-default-load-average nil)
;; show date in mode-line
(display-time)

;; line-column
;; To make the position number update correctly in all cases
(line-number-mode +1)
(column-number-mode +1)

;; ;; make displayed column number to count from 1
;; (setq column-number-indicator-zero-based nil)

;; human readable representation of file size in mode-line
(size-indication-mode +1)

(provide 'init-modeline)

;;; init-modeline.el ends here
