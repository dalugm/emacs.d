;;; init-org.el --- org configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; ORGanize.
;;

;;; Code:

(use-package org
  :bind (("C-c o b" . org-switchb)
         ("C-c o d" . org-demote-subtree)
         ("C-c o i" . org-insert-structure-template)
         ("C-c o l" . org-store-link)
         ("C-c o o" . org-open-at-point-global)
         ("C-c o p" . org-promote-subtree)
         ("C-c o t" . org-toggle-link-display))
  :custom
  ;; Make Emacs respect kinsoku rules when wrapping lines visually.
  (word-wrap-by-category t)
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-src-fontify-natively t)
  ;; Save state changes in the LOGBOOK drawer.
  (org-log-into-drawer t)
  ;; `X/Y', X means action when enters the state, Y means action when
  ;; leaves the state. Use `@' to add notes and status information
  ;; (including time), use `!' to add status information only.

  ;; | DONE(d@)   | add notes when entering                            |
  ;; | DONE(d/!)  | add status when leaving                            |
  ;; | DONE(d@/!) | add note when entering and add status when leaving |
  ;; | DONE(d@/)  | WARNING: illegal                                   |

  ;; NOTE: When leaving state A to state B, if A has a leaving action
  ;; and B has an entering action. A's leaving action won't be triggered
  ;; instead of executing B's entering action.
  (org-todo-keywords
   (quote ((sequence "TODO(t)" "STARTED(s!/!)" "HANGUP(h@)"
                     "|"
                     "DONE(d)" "ABORT(a@/!)")
           (sequence "REPORT(r)" "BUG(b@)" "KNOWNCAUSE(k@)"
                     "|"
                     "FIXED(f!)")
           (sequence "WAITING(w@/!)" "SOMEDAY(S@)" "PROJECT(P@)"
                     "|"
                     "CANCELLED(c@/!)"))))
  ;; `{}' must exist to denote this is a subscript.
  (org-use-sub-superscripts (quote {}))
  (org-export-with-sub-superscripts (quote {}))
  (org-tag-alist
   (quote (("@work" . ?w) ("@home" . ?h) ("@school" . ?s)
           ("@code" . ?c) ("TOC" . ?T) ("noexport" . ?n))))
  (org-preview-latex-process-alist
   (quote ((dvisvgm
            :programs ("xelatex" "dvisvgm")
            :description "xdv > svg"
            :message "you need to install the programs: xelatex and dvisvgm."
            :image-input-type "xdv"
            :image-output-type "svg"
            :image-size-adjust (1.7 . 1.5)
            :latex-compiler
            ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
            :image-converter
            ;; use `-e' to compute exact glyph bounding boxes
            ("dvisvgm %f -e -n -b min -c %S -o %O"))
           (dvipng
            :programs ("latex" "dvipng")
            :description "dvi > png"
            :message "you need to install the programs: latex and dvipng."
            :image-input-type "dvi"
            :image-output-type "png"
            :image-size-adjust (1.0 . 1.0)
            :latex-compiler
            ("latex -interaction nonstopmode -output-directory %o %f")
            :image-converter
            ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
           (imagemagick
            :programs ("latex" "convert")
            :description "pdf > png"
            :message "you need to install the programs: latex and imagemagick."
            :image-input-type "pdf"
            :image-output-type "png"
            :image-size-adjust (1.0 . 1.0)
            :latex-compiler
            ("pdflatex -interaction nonstopmode -output-directory %o %f")
            :image-converter
            ("convert -density %D -trim -antialias %f -quality 100 %O")))))
  (org-preview-latex-default-process 'dvisvgm)
  :config
  (defun my-org-babel-load-languages ()
    "Add src_block supported src."
    (interactive)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (calc . t)
       (shell . t)
       (C . t)
       (python . t)
       (ruby . t)
       (latex . t)
       (org . t))))

  ;; ;; After v9.2 [[https://orgmode.org/Changes.html][changelog]]
  ;; ;; Org comes with a new template expansion mechanism,
  ;; ;; `org-insert-structure-template'. Default keybinding is `\C-c\C-,'.
  ;; ;; If prefer using previous patterns, e.g. `<s',
  ;; ;; check `org-tempo.el' for more information.
  ;; (add-to-list 'org-modules 'org-tempo)

  ;; -----------------------------------------
  ;; C-c . \+1w RET ;; => <2020-05-23 Sat +1w>
  ;; C-c . \-1w RET ;; => <2020-05-23 Sat -1w>
  ;; -----------------------------------------
  (define-advice org-time-stamp (:around (fn &rest args) insert-escaped-repeater)
    (apply fn args)
    (when (string-match "\\\\\\([\\+\\-].*\\)" org-read-date-final-answer)
      (save-excursion
        (backward-char)
        (insert " "
                (string-trim-right
                 (match-string 1 org-read-date-final-answer))))))

  (defun my-org-show-current-heading-tidily ()
    "Show next entry, keeping other entries closed."
    (interactive)
    (if (save-excursion (end-of-line) (outline-invisible-p))
        (progn (org-show-entry) (outline-show-children))
      (outline-back-to-heading)
      (unless (and (bolp) (org-at-heading-p))
        (org-up-heading-safe)
        (outline-hide-subtree)
        (error "Boundary reached!"))
      (org-overview)
      (org-reveal t)
      (org-show-entry)
      (outline-show-children)))

  (keymap-global-set "C-c o o" #'my-org-show-current-heading-tidily)

  ;; https://kitchingroup.cheme.cmu.edu/blog/2016/11/06/Justifying-LaTeX-preview-fragments-in-org-mode/
  ;; Use center or right, anything else means left-justified as the default.
  (plist-put org-format-latex-options :justify 'right)

  ;; ;; Enlarge the preview magnification.
  ;; (plist-put org-format-latex-options :scale 1.5)

  (defun my--org-justify-fragment-overlay-h (beg end image imagetype)
    "Adjust the justification of a LaTeX fragment horizontally.
The justification is set by :justify in `org-format-latex-options'.
Only equations at the beginning of a line are justified.

URL `https://kitchingroup.cheme.cmu.edu/blog/2016/11/06/Justifying-LaTeX-preview-fragments-in-org-mode/'."
    (let* ((position (plist-get org-format-latex-options :justify))
           (img (create-image image 'svg t))
           (ov (car (overlays-at (/ (+ beg end) 2) t)))
           (width (car (image-display-size (overlay-get ov 'display))))
           offset)
      (cond
       ((and (eq 'center position)
             (= beg (line-beginning-position)))
        (setq offset (floor (- (/ fill-column 2)
                               (/ width 2))))
        (when (< offset 0)
          (setq offset 0))
        (overlay-put ov 'before-string (make-string offset ? )))
       ((and (eq 'right position)
             (= beg (line-beginning-position)))
        (setq offset (floor (- fill-column
                               width)))
        (when (< offset 0)
          (setq offset 0))
        (overlay-put ov 'before-string (make-string offset ? ))))))

  (advice-add 'org--make-preview-overlay :after #'my--org-justify-fragment-overlay-h)

  (defun my-org-toggle-justify-fragment-overlay-h ()
    "Toggle justify LaTeX fragment horizontally."
    (interactive)
    (if (advice-member-p #'my--org-justify-fragment-overlay-h 'org--make-preview-overlay)
        (advice-remove 'org--make-preview-overlay #'my--org-justify-fragment-overlay-h)
      (advice-add 'org--make-preview-overlay :after #'my--org-justify-fragment-overlay-h)))

  (defun my--org-justify-fragment-overlay-v (beg end &rest _args)
    "Adjust the justification of a LaTeX fragment vertically."
    (let* ((ov (car (overlays-at (/ (+ beg end) 2) t)))
           (img (cdr (overlay-get ov 'display)))
           (new-img (plist-put img :ascent 95)))
      (overlay-put ov 'display (cons 'image new-img))))

  (defun my-org-toggle-justify-fragment-overlay-v ()
    "Toggle justify LaTeX fragment vertically."
    (interactive)
    (if (advice-member-p #'my--org-justify-fragment-overlay-v 'org--make-preview-overlay)
        (advice-remove 'org--make-preview-overlay #'my--org-justify-fragment-overlay-v)
      (advice-add 'org--make-preview-overlay :after #'my--org-justify-fragment-overlay-v)))

  (defun my--org-renumber-fragment (orig-func &rest args)
    "Number equations in LaTeX fragment.

URL `https://kitchingroup.cheme.cmu.edu/blog/2016/11/07/Better-equation-numbering-in-LaTeX-fragments-in-org-mode/'."
    (let ((results '())
          (counter -1)
          equation-number)
      (setq results (cl-loop for (begin . env)
                             in (org-element-map
                                    (org-element-parse-buffer)
                                    'latex-environment
                                  (lambda (env)
                                    (cons
                                     (org-element-property :begin env)
                                     (org-element-property :value env))))
                             collect
                             (cond
                              ((and (string-match "\\\\begin{equation}" env)
                                    (not (string-match "\\\\tag{" env)))
                               (cl-incf counter)
                               (cons begin counter))
                              ((and (string-match "\\\\begin{align}" env)
                                    (string-match "\\\\notag" env))
                               (cl-incf counter)
                               (cons begin counter))
                              ((string-match "\\\\begin{align}" env)
                               (prog2
                                   (cl-incf counter)
                                   (cons begin counter)
                                 (with-temp-buffer
                                   (insert env)
                                   (goto-char (point-min))
                                   ;; \\ is used for a new line
                                   ;; Each one leads to a number
                                   (cl-incf counter (count-matches "\\\\$"))
                                   ;; unless there are nonumbers
                                   (goto-char (point-min))
                                   (cl-decf counter
                                            (count-matches "\\nonumber")))))
                              (t
                               (cons begin nil)))))
      (when (setq equation-number (cdr (assoc (point) results)))
        (setf (car args)
              (concat
               (format "\\setcounter{equation}{%s}\n" equation-number)
               (car args)))))
    (apply orig-func args))

  (defun my-org-toggle-renumber-fragment ()
    "Toggle renumber LaTeX fragment behavior."
    (interactive)
    (if (advice-member-p #'my--org-renumber-fragment 'org-create-formula-image)
        (advice-remove 'org-create-formula-image #'my--org-renumber-fragment)
      (advice-add 'org-create-formula-image :around #'my--org-renumber-fragment))))

(use-package org-clock
  :after org
  :custom
  ;; Save clock data and notes in the LOGBOOK drawer.
  (org-clock-into-drawer t)
  ;; Remove clocked tasks with 0:00 duration.
  (org-clock-out-remove-zero-time-clocks t))

(use-package org-archive
  :after org
  :custom
  (org-archive-mark-done nil)
  (org-archive-location "%s_archive::* Archive")
  :config
  (defun my-org-archive-done-tasks ()
    "Archive DONE tasks."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "/DONE" 'file)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "/ABORT" 'file)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "/CANCELLED" 'file)))

(use-package org-agenda
  :bind (("C-c o a" . org-agenda))
  :config
  (defvar my--org-task-file (concat org-directory "/task.org")
    "My org task file.")
  (defvar my--org-work-file (concat org-directory "/work.org")
    "My org work file.")
  (defvar my--org-todo-file (concat org-directory "/todo.org")
    "My org todo file.")
  (defvar my--org-inbox-file (concat org-directory "/inbox.org")
    "My org inbox file.")
  (defvar my--org-someday-file (concat org-directory "/someday.org")
    "My org file that records something may do in someday.")
  (defvar my--org-journal-file (concat org-directory "/journal.org")
    "My org journal file.")
  (defvar my--org-read-file (concat org-directory "/read.org")
    "My org reading record file.")
  (defvar my--org-bill-file (concat org-directory "/bill.org")
    "My org billing file.")
  (defvar my--org-blog-dir (concat org-directory "/blog/")
    "My org blog directory.")

  (defun my--find-month-tree ()
    "Go to current month heading."
    (let ((path (list (format-time-string "%Y %m")))
          (level 1)
          end)
      (unless (derived-mode-p 'org-mode)
        (error "Target buffer `%s' should be in org mode!"
               (current-buffer)))
      (goto-char (point-min))
      ;; Locate YEAR headline, then MONTH headline.
      (dolist (heading path)
        (let ((re (format org-complex-heading-regexp-format
                          (regexp-quote heading))))
          (if (re-search-forward re end t)
              (goto-char (line-beginning-position))
            ;; New headline.
            (when (bolp) (insert "\n"))
            (when (/= (point) (point-min)) (org-end-of-subtree t t))
            (insert (make-string level ?*) " " heading "\n")))
        (setq level (1+ level))
        (setq end (save-excursion (org-end-of-subtree t t))))
      (org-end-of-subtree)))

  ;; |                org-capture-templates common used entry               |
  ;; |--------+-------------------------------------------------------------|
  ;; | %a     | annotation, normally the link created with `org-store-link' |
  ;; | %i     | initial content, copied from the active region              |
  ;; | %^g    | tag                                                         |
  ;; | %t     | timestamp, date only                                        |
  ;; | %T     | timestamp, with date and time                               |
  ;; | %u，%U | timestamp, but inactive                                     |
  ;; | %?     | cursor location after completing the template               |
  ;; NOTE: inactive timestamp will not be added to agenda.

  (setq org-capture-templates
        `(("t" "TASK")
          ("tt" "Todo" entry
           (file+headline my--org-todo-file "Todo")
           "* TODO %^{todo}\n")
          ("td" "Daily Task" entry
           (file+headline my--org-task-file "Daily")
           "* TODO %^{task}\n   %?\n")
          ("tm" "Misc Task" entry
           (file+headline my--org-task-file "Misc")
           "* TODO %^{task}\n   %?\n")
          ("tp" "Project Task" entry
           (file+headline my--org-task-file "Project")
           "* TODO %^{project name}\n   %i\n" :clock-in t :clock-resume t)
          ("tw" "Work Task" entry
           (file+headline my--org-work-file "Work")
           "* TODO %^{task name}\n   %t\n" :clock-in t :clock-resume t)

          ("i" "INBOX")
          ("ii" "Inbox" entry
           (file+headline my--org-inbox-file "Inbox")
           "* %T - %^{inbox} %^g\n   %?\n")
          ("ie" "Event" entry
           (file+headline my--org-inbox-file "Event")
           "* %T - %^{event} %^g\n   %?\n")
          ("in" "Note" entry
           (file+headline my--org-inbox-file "Note")
           "* %^{notes} %t %^g\n   %?\n")
          ("m" "MISC")
          ("mr" "Read" entry
           (file+headline my--org-read-file "Book")
           "* TODO %^{book name}\n   %u\n" :clock-in t :clock-resume t)
          ("mb" "Bill" plain
           (file+function my--org-bill-file my--find-month-tree)
           " | %U | %^{category} | %^{desc} | %^{price} |" :kill-buffer t)
          ("ms" "Someday" entry
           (file+headline my--org-someday-file "Someday")
           "* Someday %?\n   %i\n")

          ("b" "BLOG" plain
           (file ,(concat my--org-blog-dir
                          (format-time-string "%Y-%m-%d.org")))
           ,(concat "#+startup: showall\n"
                    "#+options: toc:nil\n"
                    "#+begin_export html\n"
                    "---\n"
                    "layout     : post\n"
                    "title      : %^{title}\n"
                    "categories : %^{category}\n"
                    "tags       : %^{tag}\n"
                    "---\n"
                    "#+end_export\n"
                    "#+TOC: headlines 2\n"))

          ("j" "JOURNAL" entry
           (file+olp+datetree my--org-journal-file)
           "* - %^U - %^{heading}\n %?"))))

(use-package org-capture
  :bind (("C-c o c" . org-capture))
  :custom
  (org-agenda-files `(,org-directory)))

(use-package ox-latex
  :after ox
  :custom
  (org-latex-compiler "xelatex")
  ;; Export org in Chinese into PDF.
  ;; https://freizl.github.io/posts/2012-04-06-export-orgmode-file-in-Chinese.html
  (org-latex-pdf-process
   '("xelatex -interaction nonstopmode -output-directory %o %f"
     "xelatex -interaction nonstopmode -output-directory %o %f"
     "xelatex -interaction nonstopmode -output-directory %o %f"))
  :config
  (add-to-list 'org-latex-classes
               '("ctexart" "\\documentclass[11pt]{ctexart}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-default-class "ctexart"))

(use-package ox-md
  :after ox
  :config
  (add-to-list 'org-export-backends 'md)
  (setq org-export-coding-system 'utf-8))

(provide 'init-org)

;;; init-org.el ends here
