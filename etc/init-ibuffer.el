;;; init-ibuffer.el --- ibuffer-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Use `ibuffer' to replace `list-buffers'.
;;

;;; Code:

(with-eval-after-load 'ibuffer

  ;; Display vc status info in the ibuffer list
  (defun ibuffer-vc--state (file)
    "Return the `vc-state' for FILE, or `nil' if unregistered."
    (ignore-errors (vc-state file)))

  (defun ibuffer-vc--status-string ()
    "Return a short string to represent the current buffer's status."
    (when buffer-file-name
      (let ((state (ibuffer-vc--state buffer-file-name)))
        (if state
            (symbol-name state)
          "-"))))

  ;; Use human readable Size column instead of original one
  (define-ibuffer-column my--size
    (:name "Size" :inline t)
    (cond
      ((> (buffer-size) 1000000)
        (format "%7.1fM" (/ (buffer-size) 1000000.0)))
      ((> (buffer-size) 1000)
        (format "%7.1fk" (/ (buffer-size) 1000.0)))
      (t
        (format "%8d" (buffer-size)))))

  (define-ibuffer-column vc-status-mini
    (:name "V")
    (if buffer-file-name
        (let ((state (ibuffer-vc--state buffer-file-name)))
          (cond
            ((eq 'added state) "A")
            ((eq 'removed state) "D")
            ((eq 'up-to-date state) "U")
            ((eq 'edited state) "E")
            ((eq 'needs-update state) "N")
            ((memq state '(conflict needs-merge unlocked-changes)) "C")
            ((eq 'ignored state) "!")
            ((memq state '(() unregistered missing)) "?")))
      " "))

  (define-ibuffer-column vc-status
    (:name "VC status")
    (ibuffer-vc--status-string))

  (setq ibuffer-expert t
        ibuffer-show-empty-filter-groups nil
        ibuffer-display-summary nil)

  (setq ibuffer-saved-filter-groups
    '(("default"
        ("Dired" (mode . dired-mode))
        ("Emacs" (or (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")))
        ("Lisp" (or (mode . emacs-lisp-mode)
                    (mode . lisp-interaction-mode)))
        ("Org" (mode . org-mode))
        ("Magit" (or (mode . magit-mode)
                     (mode . magit-log-mode)
                     (mode . magit-diff-mode)
                     (mode . magit-status-mode)
                     (mode . magit-process-mode)
                     (mode . magit-revision-mode)
                     (mode . magit-submodule-list-mode)
                     (mode . forge-topic-mode)))
        ("Git" (or (mode . gitconfig-mode)
                   (mode . gitignore-mode)
                   (mode . gitattributes-mode)))
        ("C/C++" (or (mode . c-mode)
                     (mode . cc-mode)))
        ("Python" (or (mode . python-mode)
                      (mode . ipython-mode)
                      (mode . ipynb-mode)
                      (mode . ein:notebooklist-mode)
                      (mode . inferior-python-mode)))
        ("FrontEnd" (or (mode . js-mode)
                        (mode . css-mode)
                        (mode . scss-mode)
                        (mode . html-mode)
                        (mode . mhtml-mode)
                        (mode . less-css-mode)))
        ("Ruby" (or (mode . ruby-mode)
                    (mode . enh-ruby-mode)
                    (mode . inf-ruby-mode)))
        ("Sh" (mode . sh-mode))
        ("Markdown" (or (name . "^diary$")
                        (mode . markdown-mode)))
        ("ReStructText" (mode . rst-mode))
        ("Txt" (mode . text-mode)))))

  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (my--size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)))

  (defun my//ibuffer-mode-hook-setup ()
    "`ibuffer-mode' configuration."
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process))
    (ibuffer-switch-to-saved-filter-groups "default"))

  (add-hook 'ibuffer-mode-hook #'my//ibuffer-mode-hook-setup))

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

(provide 'init-ibuffer)

;;; init-ibuffer.el ends here
