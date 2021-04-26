;;; init-filetype.el --- filetype recognize -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Choose major mode according to file extension.
;;

;;; Code:

;;; functions

;; Handier way to add modes to auto-mode-alist
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use MODE for all given file PATTERNS."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun add-interpreter-mode (mode &rest patterns)
  "Add entries to `interpreter-mode-alist' to use MODE for all given file PATTERNS."
  (dolist (pattern patterns)
    (add-to-list 'interpreter-mode-alist (cons pattern mode))))

;;; Program

;; sexp
(add-auto-mode 'lisp-mode "\\.rkt\\'")
(add-auto-mode 'emacs-lisp-mode
               "\\.emacs-project\\'"
               "archive-contents\\'"
               "\\.emacs\\.bmk\\'" )
(add-auto-mode 'scheme-mode "\\.\\(sls\\|sc\\)\\'")

;; ruby
(add-auto-mode 'ruby-mode
               "\\.\\(rb\\|rake\\|rxml\\|rjs\\|irbrc\\|builder\\|ru\\|gemspec\\)$"
               "\\(Rakefile\\|Gemfile\\)$")

;; python
(add-interpreter-mode 'python-mode "python")

;; java
(add-auto-mode 'java-mode
               ;; java
               "\\.aj\\'"
               ;; makefile
               "\\.ninja$" )

;; verilog
(add-auto-mode 'verilog-mode "\\.[ds]?vh?\\'")

;; octave
(add-auto-mode 'octave-mode "\\.m$")

;; javascript
(add-auto-mode 'js-mode
               "\\.babelrc\\'"
               "\\.ja?son$"
               "\\.js\\(\\.erb\\)?\\'"
               "\\.mock.js\\'"
               "\\.pac$"
               "\\.jshintrc$")
(add-interpreter-mode 'js-mode "node")

;; snippet
(add-auto-mode 'snippet-mode "\\.yasnippet\\'")

;;; shell and conf
(add-auto-mode 'conf-mode
               "\\.[^b][^a][a-zA-Z]*rc$"
               "\\.aspell\\.en\\.pws$"
               "\\.i3/config-base$"
               "\\mimeapps\\.list$"
               "\\mimeapps\\.list$"
               "\\.editorconfig$"
               "\\.meta$"
               "\\.?muttrc$"
               "\\.mailcap$")

;; shell
(add-auto-mode 'sh-mode
               "\\.bash\\(_profile\\|rc\\.local\\|rc\\)?$"
               "\\.zsh\\(_profile\\|rc\\.local\\|rc\\)?$"
               "\\.env$")

;;; text
(add-auto-mode 'text-mode
               "TAGS\\'"
               "\\.ctags\\'")

;; pyim
(add-auto-mode 'text-mode "\\.pyim\\'")

(provide 'init-filetype)

;;; init-filetype.el ends here
