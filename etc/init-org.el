;;; init-org.el --- org configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; ORG is a powerful tool in Emacs.
;;

;;; Code:

;; Keybindings
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o c") #'org-capture)
(global-set-key (kbd "C-c o b") #'org-switchb)

(with-eval-after-load 'org

  ;; ---------------------------------------------------------
  ;; agenda
  ;; ---------------------------------------------------------
  (setq org-agenda-files `(,org-directory))

  ;; ---------------------------------------------------------
  ;; capture
  ;; ---------------------------------------------------------
  (setq org-default-notes-file (concat org-directory "/notes.org"))

  ;; useful initials
  (defvar my--org-task-file (concat org-directory "/task.org")
    "Org task file")
  (defvar my--org-todo-file (concat org-directory "/todo.org")
    "Org todo file.")
  (defvar my--org-inbox-file (concat org-directory "/inbox.org")
    "Org inbox file.")
  (defvar my--org-someday-file (concat org-directory "/someday.org")
    "Org file that records something may do in someday.")
  (defvar my--org-journal-file (concat org-directory "/journal.org")
    "Org journal file.")
  (defvar my--org-read-file (concat org-directory "/read.org")
    "Org reading record file.")
  (defvar my--org-bill-file (concat org-directory "/bill.org")
    "Org billing file.")
  (defvar my--org-blog-dir (concat org-directory "/blog/")
    "My org blog directory.")

  (defun my//get-year-and-month ()
    "Get current year and month."
    (list (format-time-string "%Y") (format-time-string "%m")))

  (defun my//find-month-tree ()
    "Go to current month heading."
    (let ((path (my//get-year-and-month))
          (level 1)
          end)
      (unless (derived-mode-p 'org-mode)
        (error "Target buffer \"%s\" should be in Org mode"
               (current-buffer)))
      ;; 移动到 buffer 的开始位置
      (goto-char (point-min))
      ;; 先定位表示年份的 headline，再定位表示月份的 headline
      (dolist (heading path)
        (let ((re (format org-complex-heading-regexp-format
                    (regexp-quote heading)))
              (cnt 0))
          (if (re-search-forward re end t)
              ;; 如果找到了 headline 就移动到对应的位置
              (goto-char (point-at-bol))
            ;; 否则就新建一个 headline
            (progn
              (or (bolp) (insert "\n"))
              (when (/= (point) (point-min)) (org-end-of-subtree t t))
              (insert (make-string level ?*) " " heading "\n"))))
        (setq level (1+ level))
        (setq end (save-excursion (org-end-of-subtree t t))))
      (org-end-of-subtree)))

  ;; |       org-capture-templates 常用选项              |
  ;; |--------+------------------------------------------|
  ;; | %a     | 注释，通常是由 org-store-link 创建的链接 |
  ;; | %i     | 初始化内容，当记忆时区域被 C-u 调用      |
  ;; | %g     | tag 标签                                 |
  ;; | %t     | 时间戳，只带有日期                       |
  ;; | %T     | 带有日期和时间的时间戳                   |
  ;; | %u，%U | 同上，但是时间戳不激活                   |
  ;; | %?     | 输入完成后光标所在位置                   |

  ;; NOTE: 未激活的时间戳不会加入到 agenda 中

  (setq org-capture-templates
    `(;; tasks
       ("t" "TASK")
       ("tt" "Todo" entry (file+headline my--org-todo-file "Todo")
         "* TODO %^{todo}\n")
       ("td" "Daily Task" entry (file+headline my--org-task-file "Daily")
         "* TODO %^{task}\n   %?\n")
       ("tp" "Project Task" entry (file+headline my--org-task-file "Project")
         "* TODO %^{project name}\n   %i\n" :clock-in t :clock-resume t)
       ("tw" "Work Task" entry (file+headline my--org-task-file "Work")
         "* TODO %^{task name}\n   %t\n" :clock-in t :clock-resume t)
       ;; inbox
       ("i" "INBOX")
       ("ii" "Inbox" entry (file+headline my--org-inbox-file "Inbox")
         "* %T - %^{inbox} %^g\n   %?\n")
       ("ie" "Event" entry (file+headline my--org-inbox-file "Event")
         "* %T - %^{event} %^g\n   %?\n")
       ("in" "Note" entry (file+headline my--org-inbox-file "Note")
         "* %^{notes} %t %^g\n   %?\n")
       ;; misc
       ("m" "MISC")
       ("mr" "Read" entry (file+headline my--org-read-file "Book")
         "* TODO %^{book name}\n   %u\n" :clock-in t :clock-resume t)
       ("mb" "Bill" plain (file+function my--org-bill-file my//find-month-tree)
         " | %U | %^{category} | %^{desc} | %^{price} |" :kill-buffer t)
       ("ms" "Someday" entry (file+headline my--org-someday-file "Someday")
         "* Someday %?\n   %i\n")

       ("b" "BLOG" plain (file ,(concat my--org-blog-dir
                                  (format-time-string "%Y-%m-%d.org")))
         ,(concat "#+startup: showall\n"
            "#+options: toc:nil\n"
            "#+begin_export html\n"
            "---\n"
            "layout     : post\n"
            "title      : %^{标题}\n"
            "categories : %^{类别}\n"
            "tags       : %^{标签}\n"
            "---\n"
            "#+end_export\n"
            "#+TOC: headlines 2\n"))
       ("j" "JOURNAL" entry (file+olp+datetree my--org-journal-file)
         "* - %^U - %^{heading}\n %?")))

  ;; ---------------------------------------------------------
  ;; Enhance org
  ;; ---------------------------------------------------------
  ;; make Emacs respect kinsoku rules when wrapping lines visually
  (setq word-wrap-by-category t)

  (defun my/org-demote-or-promote (&optional is-promote)
    "Demote or promote current org tree according to IS-PROMOTE."
    (interactive "P")
    (unless (region-active-p)
      (org-mark-subtree))
    (if is-promote (org-do-promote) (org-do-demote)))

  ;;---------------------------
  ;; C-c . \+1w RET ;; => <2020-05-23 Sat +1w>
  ;; C-c . \-1w RET ;; => <2020-05-23 Sat -1w>
  ;;---------------------------
  (define-advice org-time-stamp (:around (fn &rest args) insert-escaped-repeater)
    (apply fn args)
    (when (string-match "\\\\\\([\\+\\-].*\\)" org-read-date-final-answer)
      (save-excursion
        (backward-char)
        (insert " "
          (string-trim-right
            (match-string 1 org-read-date-final-answer))))))

  (defun my/org-show-current-heading-tidily ()
    "Show next entry, keeping other entries closed."
    (interactive)
    (if (save-excursion (end-of-line) (outline-invisible-p))
        (progn (org-show-entry) (show-children))
      (outline-back-to-heading)
      (unless (and (bolp) (org-on-heading-p))
        (org-up-heading-safe)
        (hide-subtree)
        (error "Boundary reached"))
      (org-overview)
      (org-reveal t)
      (org-show-entry)
      (show-children)))

  (global-set-key (kbd "C-c o o") #'my/org-show-current-heading-tidily)

  ;; ---------------------------------------------------------
  ;; babel
  ;; ---------------------------------------------------------
  ;; fontify source code in code blocks
  ;; default value is nil after Emacs v24.1
  (setq org-src-fontify-natively t)

  ;; add SRC_BLOCK supported src
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (calc . t)
      (shell . t)
      (C . t)
      (python . t)
      (ruby . t)
      (latex . t)
      (org . t)))

  ;; ---------------------------------------------------------
  ;; TODO
  ;; ---------------------------------------------------------
  ;; format `X/Y', X means action when enters the state, Y means action when leaves the state
  ;; use `@' to add notes and status information(including time)
  ;; use `!' to add status information only

  ;; | DONE(d@)   | add notes when entering                            |
  ;; | DONE(d/!)  | add status when leaving                            |
  ;; | DONE(d@/!) | add note when entering and add status when leaving |
  ;; | DONE(d@/)  | WARNING: illegal                                   |

  ;; NOTE: when leaving state A to state B, if A has a leaving action and B has an entering action
  ;; A's leaving action won't be triggered instead of executing B's entering action

  ;; ;; use `\C-c\C-t' to toggle state
  ;; default value is t after org v8
  ;; (setq org-use-fast-todo-selection t)

  (setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s!/!)" "HANGUP(h@)" "|" "DONE(d)" "ABORT(a@/!)")
                            (sequence "REPORT(r)" "BUG(b@)" "KNOWNCAUSE(k@)" "|" "FIXED(f!)")
                            (sequence "WAITING(w@/!)" "SOMEDAY(S@)" "PROJECT(P@)" "|" "CANCELLED(c@/!)")))
  (setq org-todo-keyword-faces '(("TODO"    . "red")
                                 ("STARTED" . "yellow")
                                 ("DONE"    . "green")
                                 ("ABORT"   . "grey")))

  ;; ---------------------------------------------------------
  ;; clock
  ;; ---------------------------------------------------------
  ;; Save clock data and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Save state changes in the LOGBOOK drawer
  (setq org-log-into-drawer t)
  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)

  ;; ---------------------------------------------------------
  ;; archive
  ;; ---------------------------------------------------------
  (defun my/org-archive-done-tasks ()
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
      "/CANCELLED" 'file))

  (setq org-archive-mark-done nil)
  (setq org-archive-location "%s_archive::* Archive")

  ;; ---------------------------------------------------------
  ;; export
  ;; ---------------------------------------------------------
  (my|ensure 'ox-md)
  (my|ensure 'ox-latex)
  (add-to-list 'org-export-backends 'md)
  (add-to-list 'org-export-backends 'odt)
  (setq org-export-coding-system 'utf-8
        org-tags-column 80)

  (defun my/org-export-docx ()
    "Export org file as docx."
    (interactive)
    (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
          (template-file (concat (file-name-as-directory my-optional-d)
                                 "template.docx")))
      (shell-command (format "pandoc %s -o %s --reference-doc=%s" (buffer-file-name) docx-file template-file))
      (message "Convert finish: %s." docx-file)))

  ;; ---------------------------------------------------------
  ;; LaTeX
  ;; ---------------------------------------------------------
  (with-eval-after-load 'ox-latex
    ;; 放大预览倍数
    (plist-put org-format-latex-options :scale 1.5)
    ;; export org-mode in Chinese into PDF
    ;; https://freizl.github.io/posts/2012-04-06-export-orgmode-file-in-Chinese.html
    (setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))
    (add-to-list 'org-latex-classes
      '("ctexart" "\\documentclass[11pt]{ctexart}"
        ("\\section{%s}" . "\\section*{%s}")
        ("\\subsection{%s}" . "\\subsection*{%s}")
        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
        ("\\paragraph{%s}" . "\\paragraph*{%s}")
        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (setq org-latex-default-class "ctexart")
    ;; Compared to `pdflatex', `xelatex' supports unicode and can use system's font
    (setq org-latex-compiler "xelatex"))

  ;; ---------------------------------------------------------
  ;; misc
  ;; ---------------------------------------------------------
  (global-set-key (kbd "C-c o l") #'org-store-link)
  (global-set-key (kbd "C-c o i") #'org-insert-structure-template)

  ;; ;; after v9.2 [[https://orgmode.org/Changes.html][changlog]]
  ;; ;; Org comes with a new template expansion mechanism `org-insert-structure-template'
  ;; ;; Default keybinding is `\C-c\C-,'
  ;; ;; if prefer using previous patterns, e.g. <s, goto `org-tempo.el' for more information
  ;; ;; NOTE: I'm using `yasnippet' to handle this.
  ;; (add-to-list 'org-modules 'org-tempo)

  ;; 让中文也可以不加空格就使用行内格式
  (setcar (nthcdr 0 org-emphasis-regexp-components) " \t('\"{[:nonascii:]")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "- \t.,:!?;'\")}\\[[:nonascii:]")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (org-element-update-syntax)
  ;; 规定上下标必须加 {}，否则中文使用下划线时它会以为是两个连着的下标
  (setq org-use-sub-superscripts "{}")

  (setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("@school" . ?s)
                        ("@code" . ?c) ("TOC" . ?T) ("noexport" . ?n))))

(provide 'init-org)

;;; init-org.el ends here
