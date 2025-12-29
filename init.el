;;; init.el --- `user-init-file' -*- lexical-binding: t; -*-

(borg-report-load-duration)

(progn					; startup
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq ring-bell-function #'ignore)
  (defun my--initial-scratch-message ()
    "Customize `initial-scratch-message'."
    (format
     ;; Comment first line and add two new lines in the end
     ";; %s\n\n"
     (replace-regexp-in-string
      ;; Comment each line below first line
      "\n"
      "\n;; "
      (if-let* ((fortune-prog (executable-find "fortune")))
          (replace-regexp-in-string
           ;; Remove extra escape sequences
           (rx (or (seq ?\n eol)
                   (seq ?\C-\[ ?\[ (0+ digit) ?m)))
           ""
           (shell-command-to-string fortune-prog))
        (string-join
         '("Now, trailblazers"
           "Keep credos in mind"
           "(I won't say it twice!)"
           "One! Stop staying within the lines"
           "Two! We always align"
           "Three! Even if we don't gain the upper hand, we'll fight for right"
           "Four! Never care a rap for hindsight"
           "Five! Let us light the night"
           "Six! Even when there are wheels within wheels, go ahead!"
           "Get it pulverized")
         "\n")))))

  (setq initial-scratch-message (my--initial-scratch-message))

  (prefer-coding-system 'utf-8))

;; Ensure saved customizations are applied correctly during startup
(load
 (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
 t t)

(eval-and-compile			; `use-package'
  (setopt use-package-enable-imenu-support t)
  ;; (setopt use-package-verbose t)
  (require 'use-package))

(use-package compat)

(use-package no-littering
  :config
  ;; Use shortened filenames
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=56123
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-etc-directory))))

(use-package auto-compile
  :config
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t))

(use-package epkg
  :defer t)

(use-package epkg-marginalia
  :after (epkg marginalia)
  :config
  (setcar (alist-get 'package marginalia-annotators)
          #'epkg-marginalia-annotate-package))

(use-package server
  :preface
  (defcustom my-run-emacs-as-a-server nil
    "Non-nil means to run Emacs as a server process, which allows
access from `emacsclient'."
    :group 'convenience
    :type 'boolean)
  :if my-run-emacs-as-a-server
  :config
  (run-with-idle-timer 3 nil
                       (lambda ()
                         "Run Emacs as a server process."
                         (unless (server-running-p)
                           (message "Starting a server...")
                           (server-start)))))

(use-package emacs
  :preface
  (defcustom my-use-gbk-dos-coding-system nil
    "Non-nil means using GBK-DOS coding system."
    :group 'convenience
    :type 'boolean)

  (defcustom my-http-proxy "127.0.0.1:1080"
    "HTTP proxy."
    :group 'convenience
    :type 'string)

  (defcustom my-socks-proxy
    (list
     (if (string-match-p "Microsoft" (shell-command-to-string "uname -a"))
         (if (file-exists-p "/etc/resolv.conf")
             (shell-command-to-string
              "cat /etc/resolv.conf | grep nameserver | awk '{ printf $2 }'")
           "0.0.0.0")
       "127.0.0.1")
     1080)
    "SOCKS proxy."
    :type '(list (string  :tag "Host")
                 (integer :tag "Port"))
    :group 'convenience)

  (defcustom my-pangu-spacing-excluded-puncuation
    "，。！？、；：‘’“”『』「」【】（）《》"
    "Excluded puncuation when pangu spacing buffer."
    :type 'string
    :group 'convenience)

  (defcustom my-pangu-spacing-regexp
    (rx-to-string
     `(or (and (or (group-n 3 (any ,my-pangu-spacing-excluded-puncuation))
                   (group-n 1 (or (category chinese-two-byte)
                                  (category japanese-hiragana-two-byte)
                                  (category japanese-katakana-two-byte)
                                  (category korean-hangul-two-byte))))
               (group-n 2 (in "a-zA-Z0-9")))
          (and (group-n 1 (in "a-zA-Z0-9"))
               (or (group-n 3 (any ,my-pangu-spacing-excluded-puncuation))
                   (group-n 2 (or (category chinese-two-byte)
                                  (category japanese-hiragana-two-byte)
                                  (category japanese-katakana-two-byte)
                                  (category korean-hangul-two-byte))))))
     t)
    "Regexp to find Chinese character around English character.

Group 1 contains the character before the potential pangu
spacing, and group 2 the character after that. A space is needed
when both group 1 and group 2 are non-nil. Group 3 exists as a
workaround for excluded puncuation.

Since rx does not support matching text that satisfy two regexp
at the same time (we want to match all Chinese two byte
characters, but not the punctuation), we first try to match
excluded puncuation, then the characters that need
pangu-spacing. The excluded puncuation will be matched to group
3, and shortcut the matching for Chinese characters.  Thus group
1 and group 2 will both be non-nil when a pangu space is needed."
    :group 'convenience
    :type 'regexp)

  (defun my-pangu-spacing-current-buffer ()
    "Pangu space current buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward my-pangu-spacing-regexp nil t)
        (when (and (match-beginning 1)
                   (match-beginning 2))
          (replace-match "\\1 \\2" nil nil)
          (backward-char)))))

  (defun my-delete-invisible-chars ()
    "Query and replace some invisible Unicode chars.

The chars replaced are:
 ZERO WIDTH NO-BREAK SPACE (65279, #xfeff)
 ZERO WIDTH SPACE (codepoint 8203, #x200b)
 RIGHT-TO-LEFT MARK (8207, #x200f)
 RIGHT-TO-LEFT OVERRIDE (8238, #x202e)
 LEFT-TO-RIGHT MARK ‎(8206, #x200e)
 OBJECT REPLACEMENT CHARACTER (65532, #xfffc)

Begin at buffer beginning, respects `narrow-to-region'.

URL `http://xahlee.info/emacs/emacs/elisp_unicode_replace_invisible_chars.html'
Version: 2018-09-07 2022-09-13."
    (interactive)
    (save-excursion
      (let ((case-replace nil)
            (case-fold-search nil))
        (goto-char (point-min))
        (while (re-search-forward
                "\ufeff\\|\u200b\\|\u200f\\|\u202e\\|\u200e\\|\ufffc"
                nil t)
          (replace-match "")))))

  (defun my-delete-blank-lines ()
    "Delete blank lines.
When region is active, delete the blank lines in region only."
    (interactive)
    (save-excursion
      (let ((regexp "^[[:space:]]*$"))
        (if (use-region-p)
            (delete-matching-lines regexp
                                   (region-beginning)
                                   (region-end))
          (delete-matching-lines regexp
                                 (point-min)
                                 (point-max))))))

  (defun my-delete-visual-blank-lines ()
    "Delete all visual blank lines."
    (interactive)
    (save-excursion
      (save-restriction
        (narrow-to-region (window-start) (window-end))
        (delete-matching-lines "^[[:space:]]*$"
                               (point-min)
                               (point-max)))))

  (defun my-toggle-two-split-window ()
    "Toggle two window layout vertically or horizontally."
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges  (window-edges (selected-window)))
               (next-win-edges  (window-edges (next-window)))
               (this-win-2nd    (not (and (<= (car this-win-edges)
                                              (car next-win-edges))
                                          (<= (cadr this-win-edges)
                                              (cadr next-win-edges)))))
               (splitter (if (= (car this-win-edges)
                                (car (window-edges (next-window))))
                             #'split-window-horizontally
                           #'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (when this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (when this-win-2nd (other-window 1))))
      (user-error "There should be only two windows in current frame")))

  (defun my-rotate-windows ()
    "Rotate windows in clock-wise direction."
    (interactive)
    (cond
     ((not (> (count-windows) 1))
      (user-error "Cannot rotate a single window"))
     (t
      (let ((i 1)
            (window-num (count-windows)))
        (while (< i window-num)
          (let* ((w1 (elt (window-list) i))
                 (w2 (elt (window-list) (+ (% i window-num) 1)))
                 (b1 (window-buffer w1))
                 (b2 (window-buffer w2))
                 (s1 (window-start w1))
                 (s2 (window-start w2)))
            (set-window-buffer w1 b2)
            (set-window-buffer w2 b1)
            (set-window-start w1 s2)
            (set-window-start w2 s1)
            (cl-incf i)))))))

  (defun my-toggle-full-window()
    "Toggle full view of selected window."
    (interactive)
    (if (window-parent)
        (delete-other-windows)
      (winner-undo)))

  (defun my-set-window-margins (margin)
    "Set the MARGIN of the current window."
    (interactive "nMargin Value: ")
    (set-window-margins (selected-window) margin margin))

  (defun my-show-http-proxy ()
    "Show http/https proxy."
    (interactive)
    (if (bound-and-true-p url-proxy-services)
        (message "Current HTTP proxy is \"%s\"." my-http-proxy)
      (message "No HTTP proxy.")))

  (defun my-enable-http-proxy ()
    "Enable HTTP/HTTPS proxy."
    (interactive)
    (setopt url-proxy-services
            `(("http" . ,my-http-proxy)
              ("https" . ,my-http-proxy)
              ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
    (setenv "http_proxy" (format "http://%s" my-http-proxy))
    (setenv "https_proxy" (format "http://%s" my-http-proxy))
    (my-show-http-proxy))

  (defun my-disable-http-proxy ()
    "Disable HTTP/HTTPS proxy."
    (interactive)
    (setopt url-proxy-services nil)
    (setenv "http_proxy" "")
    (setenv "https_proxy" "")
    (my-show-http-proxy))

  (defun my-toggle-http-proxy ()
    "Toggle HTTP/HTTPS proxy."
    (interactive)
    (if (bound-and-true-p url-proxy-services)
        (my-disable-http-proxy)
      (my-enable-http-proxy)))

  (defun my-show-socks-proxy ()
    "Show SOCKS proxy."
    (interactive)
    (if (eq url-gateway-method 'socks)
        (message "Current SOCKS%d proxy is \"%s:%d\"."
                 (cadddr socks-server)
                 (cadr   socks-server)
                 (caddr  socks-server))
      (message "No SOCKS proxy.")))

  (defun my-enable-socks-proxy ()
    "Enable SOCKS proxy."
    (interactive)
    (require 'socks)
    (let ((host (car  my-socks-proxy))
          (port (cadr my-socks-proxy)))
      (setopt url-gateway-method 'socks
              socks-server `("Default server" ,host ,port 5)
              socks-noproxy '("localhost"))
      (setenv "all_proxy" (format "socks5://%s:%s" host port)))
    (my-show-socks-proxy))

  (defun my-disable-socks-proxy ()
    "Disable SOCKS proxy."
    (interactive)
    (setopt url-gateway-method 'native)
    (setenv "all_proxy" "")
    (my-show-socks-proxy))

  (defun my-toggle-socks-proxy ()
    "Toggle SOCKS proxy."
    (interactive)
    (if (eq url-gateway-method 'socks)
        (my-disable-socks-proxy)
      (my-enable-socks-proxy)))

  (defcustom my-search-engine-alist
    '((baidu         . "https://www.baidu.com/s?wd=")
      (bilibili      . "https://search.bilibili.com/all?keyword=")
      (bing          . "https://www.bing.com/search?q=")
      (duckduckgo    . "https://www.duckduckgo.com/?q=")
      (github        . "https://www.github.com/search?q=")
      (google        . "https://www.google.com/search?q=")
      (longman       . "https://www.ldoceonline.com/dictionary/")
      (stackoverflow . "https://stackoverflow.com/search?q=")
      (vocabulary    . "https://www.vocabulary.com/dictionary/")
      (wikipedia     . "https://www.wikipedia.org/wiki/Special:Search?go=Go&search=")
      (youtube       . "https://www.youtube.com/results?search_query=")
      (zhihu         . "https://www.zhihu.com/search?type=content&q="))
    "An alist of all the engines you can search by.
Key is a symbol as the name, value is a plist specifying the search url."
    :type '(alist :key-type symbol :value-type string)
    :group 'convenience)

  (defun my-search-online (&optional search-engine)
    "Search a query or region if any by using SEARCH-ENGINE."
    (interactive (list
                  (completing-read "Choose a search engine: "
                                   (mapcar #'car my-search-engine-alist))))
    (let ((search-url (alist-get (intern search-engine)
                                 my-search-engine-alist
                                 nil nil #'equal)))
      (browse-url
       (url-encode-url
        (concat search-url
                (if mark-active
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string
                   (message "%s Search: " (capitalize search-engine)))))))))

  (defun my-rename-this-file (&optional arg)
    "Rename both current buffer and file.
With a prefix ARG, rename based on current name."
    (interactive "P")
    (let ((filename (buffer-file-name)))
      (unless filename
        (user-error "Buffer `%s' is not visiting a file" filename))
      (let ((new-name (read-string
                       "New name: "
                       (when arg (file-name-nondirectory filename)))))
        (progn
          (when (file-exists-p filename)
            (rename-file filename new-name +1))
          (set-visited-file-name new-name)
          (rename-buffer new-name)))
      (save-buffer)))

  (defun my-copy-file-name ()
    "Copy file name to clipboard."
    (interactive)
    (let ((filename (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
      (if filename
          (progn
            (kill-new (file-name-nondirectory filename))
            (message "Copied `%s'." (file-name-nondirectory filename)))
        (user-error "Current buffer is not attached to a file"))))

  (defun my-browse-this-file ()
    "Open current file as a URL using `browse-url'."
    (interactive)
    (let ((file-name (buffer-file-name)))
      (if (and (fboundp 'tramp-tramp-file-p)
               (tramp-tramp-file-p file-name))
          (user-error "Cannot open tramp file")
        (browse-url (concat "file://" file-name)))))

  (defun my-open-externally (file)
    "Open FILE or url using system's default application."
    (interactive "sOpen externally: ")
    (unless (string-match-p "\\`[a-z]+://" file)
      (setq file (expand-file-name file)))
    (message "Opening `%s' externally..." file)
    (if (and (eq system-type 'windows-nt)
             (fboundp 'w32-shell-execute))
        (w32-shell-execute "open" file)
      (call-process (pcase system-type
                      ('darwin "open")
                      ('cygwin "cygstart")
                      (_ "xdg-open"))
                    nil 0 nil file)))

  (defun my-open-file-externally (file)
    "Open FILE using system's default application."
    (interactive "fOpen externally: ")
    (my-open-externally file))

  (defun my-delete-this-file ()
    "Delete current file, and kill the buffer."
    (interactive)
    (unless (buffer-file-name)
      (user-error "No file is currently being edited"))
    (when (yes-or-no-p
           (format-message "Really delete `%s'?"
                           (file-name-nondirectory buffer-file-name)))
      (delete-file (buffer-file-name))
      (kill-this-buffer)))

  (defun my-delete-file (file)
    "Delete FILE under current working directory."
    (interactive "sFile name: ")
    (shell-command
     (format "find . -depth -name %s -print0 | xargs -0 rm" file))
    (message "`%s' under current working directory deleted." file))

  (defun my--sudo-file-path (file)
    "Get FILE's path with sudo."
    (let ((host (or (file-remote-p file 'host) "localhost")))
      (concat "/" (when (file-remote-p file)
                    (concat (file-remote-p file 'method) ":"
                            (if-let* ((user (file-remote-p file 'user)))
                                (concat user "@" host)
                              host)
                            "|"))
              "sudo:root@" host
              ":" (or (file-remote-p file 'localname)
                      file))))

  (defun my-sudo-edit-file ()
    "Edit current file as root."
    (interactive)
    (find-file
     (my--sudo-file-path
      (or buffer-file-name
          (when (or (derived-mode-p 'dired-mode)
                    (derived-mode-p 'wdired-mode))
            default-directory)))))

  (defun my-sudo-find-file (file)
    "Open FILE as root."
    (interactive "FOpen file as root: ")
    (find-file (my--sudo-file-path file)))

  (defun my-toggle-selective-display (column)
    "Quick and dirty code folding with COLUMN.

URL `https://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/'"
    (interactive "P")
    (set-selective-display
     (if selective-display nil (or column 1))))

  (defun my-occur-dwim ()
    "Call `occur' with a sane default."
    (interactive)
    (let* ((default-input (if (use-region-p)
                              (buffer-substring-no-properties
                               (region-beginning)
                               (region-end))
                            (when-let* ((sym (thing-at-point 'symbol t)))
                              (regexp-quote sym))))
           (prompt (if default-input
                       (format
                        "List lines matching (default %s): "
                        default-input)
                     "List lines matching: ")))
      (occur (read-regexp prompt default-input))))

  (defun my-hide-dos-eol ()
    "Do not show CR in files with mixed UNIX and DOS line endings."
    (interactive)
    (unless buffer-display-table
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\C-m []))

  (defun my-dos2unix ()
    "Replace DOS eolns CR LF with Unix eolns CR."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward (string ?\C-m) nil t)
        (replace-match "" nil t))))

  (defun my-load-theme (x)
    "Disable current theme and load theme X."
    (interactive (list
                  (completing-read
                   "Choose a theme: "
                   (mapcar #'symbol-name (custom-available-themes)))))
    (condition-case _
        (progn
          (mapc #'disable-theme custom-enabled-themes)
          (load-theme (intern x) t))
      (error "Problem loading theme %s" x)))

  (defun my-load-default-theme ()
    "Load default Emacs theme."
    (interactive)
    (mapc #'disable-theme custom-enabled-themes))

  (defun my-kill-other-buffers-without-special-ones ()
    "Kill all buffers but the current one, while not messing with special ones."
    (interactive)
    (when (y-or-n-p "Kill all buffers but current with special ones? ")
      (seq-do #'kill-buffer
              (remove
               (current-buffer)
               (seq-filter #'buffer-file-name (buffer-list))))))

  (defun my-kill-other-buffers-with-special-ones ()
    "Keep all buffers (include special buffers) but the current one.

URL `https://stackoverflow.com/questions/3417438/closing-all-other-buffers-in-emacs'"
    (interactive)
    (when (y-or-n-p "Kill all buffers but current one? ")
      (mapc #'kill-buffer (cdr (buffer-list (current-buffer))))))

  (defun my-insert-date (&optional arg)
    "Insert current date at point.

Without ARG, use full ISO 8601 format.
With a `\\[universal-argument]' prefix argument ARG, use unix timestamp.
With a `\\[universal-argument] \\[universal-argument]' prefix \
argument ARG, use common timestamp.
With a `\\[universal-argument] \\[universal-argument] \
\\[universal-argument]' prefix argument ARG, use locale's timestamp."
    (interactive "P")
    (let ((format (cond
                   ((not arg) "%FT%T%z")
                   ((equal arg '(4)) "%s")
                   ((equal arg '(16)) "%a %b %e %T %Y %z")
                   ((equal arg '(64)) "%c"))))
      (insert (format-time-string format))))

  (defun my-insert-user-information (arg)
    "Insert user information at point.

Without ARG, insert user's name and email.
With a `\\[universal-argument]' prefix argument ARG, insert email only.
With a `\\[universal-argument] \\[universal-argument]' prefix \
argument ARG, insert name only."
    (interactive "P")
    (let ((format (cond
                   ((not arg) (concat user-full-name " <"
                                      user-mail-address ">"))
                   ((equal arg '(4)) user-mail-address)
                   ((equal arg '(16)) user-full-name))))
      (insert format)))

  (defun my-print-ascii-table (&optional arg)
    "Print the ASCII table.

Default print to 256.  With a prefix ARG, print to specified
number."
    (interactive "P")
    (let ((num (if arg
                   (read-number "Input a number: ")
                 256))
          (i 0))
      (switch-to-buffer "*ASCII*")
      (erase-buffer)
      (insert (format "ASCII characters up to number %d.\n" num))
      (while (< i num)
        (cl-incf i)
        (insert (format "%4d %c\n" i i)))
      (special-mode)
      (goto-char (point-min))))

  (defun my-pingan-emacs ()
    "建议击毙, @平安 Emacs."
    (interactive)
    (message
     (let ((list '()))
       (mapatoms (lambda (sym)
                   (when (special-form-p sym)
                     (push sym list))))
       (mapconcat (lambda (sym)
                    (format "%s 平安" sym))
                  list
                  "，"))))

  :init
  ;; Handle large files
  ;; https://emacs-china.org/t/topic/25811/9
  (setq bidi-display-reordering nil
	bidi-inhibit-bpa t
	long-line-threshold 1000
	large-hscroll-threshold 1000
        large-file-warning-threshold (* 100 1000 1000)
	syntax-wholeline-max 1000)

  ;; Nice scrolling
  (setq scroll-margin 0
        scroll-conservatively 100000
        scroll-preserve-screen-position 1)

  ;; Use y/n instead of yes/no
  (setq use-short-answers t)

  ;; Repeating C-SPC after popping mark pops it again
  (setq set-mark-command-repeat-pop t)

  ;; Make mouse clicks more precise
  (setq mouse-prefer-closest-glyph t)

  ;; Indent with spaces
  (setq-default indent-tabs-mode nil)

  ;; Pass `C-u' to `recenter' to put point in the window's center
  (setq next-error-recenter '(4))

  ;; Do NOT make backups of files, not safe
  ;; https://github.com/joedicastro/dotfiles/tree/master/emacs
  (setq auto-save-default nil
	make-backup-files nil)

  ;; Enable dangerous commands
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)

  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  (put 'erase-buffer 'disabled nil)

  (when my-use-gbk-dos-coding-system
    (add-to-list 'process-coding-system-alist
                 '("[rR][gG]" . (utf-8 . gbk-dos))))

  :bind

  (("C-c f b"   . my-browse-this-file)
   ("C-c f c"   . my-copy-file-name)
   ("C-c f d"   . my-delete-this-file)
   ("C-c f D"   . my-delete-file)
   ("C-c f o"   . my-open-file-externally)
   ("C-c f r"   . my-rename-this-file)
   ("C-c f s"   . my-sudo-edit-file)
   ("C-c f S"   . my-sudo-find-file)
   ("C-c s i"   . imenu)
   ("C-c s g"   . grep)
   ("C-c t f f" . toggle-frame-fullscreen)
   ("C-c t f m" . toggle-frame-maximized)
   ("C-c t j"   . toggle-truncate-lines)
   ("C-c t k"   . visual-line-mode)
   ("C-c t p h" . my-toggle-http-proxy)
   ("C-c t p H" . my-show-http-proxy)
   ("C-c t t"   . load-theme)
   ("C-c w f"   . my-toggle-full-window)
   ("C-c w r"   . my-rotate-windows)
   ("C-c w t"   . my-toggle-two-split-window)
   ("C-c m 1"   . my-insert-date)
   ("C-c m 2"   . my-insert-user-information)
   ("C-c m d"   . my-delete-blank-lines)
   ("C-c m D"   . my-delete-visual-blank-lines)
   ("C-c m e" .   my-open-externally)
   ("C-c m h"   . my-toggle-selective-display)
   ("C-c m o"   . my-occur-dwim)
   ("C-c m k"   . my-kill-other-buffers-without-special-ones)
   ("C-c m K"   . my-kill-other-buffers-with-special-ones)
   ("C-c m t"   . my-load-theme)
   ("C-c m T"   . my-load-default-theme)
   ("C-c m p"   . my-pangu-spacing-current-buffer)
   ("C-c m x"   . execute-extended-command)))

(use-package ffap
  :bind (("C-x C-o" . ffap)))

(use-package help
  :bind (:map help-map
              ("A" . apropos)
              ("C-f" . find-function)
              ("C-k" . find-function-on-key)
              ("C-v" . find-variable)
              ("C-l" . find-library)
              ("C-i" . info-display-manual)))

(use-package isearch
  :bind
  (("C-M-s" . isearch-forward-regexp)
   ("C-M-r" . isearch-backward-regexp)
   (:map isearch-mode-map
         ("C-o" . isearch-occur))))

(use-package simple
  :init
  (define-advice delete-indentation (:around (fn &rest args) chinese)
    "Add Chinese characters support for `fixup-whitespace'.

Use `cl-letf' to change the behavior of `fixup-whitespace' only when
called from `delete-indentation'."
    (cl-letf (((symbol-function #'fixup-whitespace)
               (lambda ()
                 (save-excursion
                   (delete-horizontal-space)
                   (if (or (looking-at "^\\|\\s)")
                           (save-excursion (forward-char -1)
                                           (looking-at "\\cc\\|$\\|\\s(\\|\\s'")))
                       nil
                     (insert ?\s))))))
      (apply fn args)))
  :bind
  (;; Zero width space
   ("C-c 8 z" . (lambda () (interactive) (insert-char #x200b)))
   ;; Ideographic space
   ("C-c 8 i" . (lambda () (interactive) (insert-char #x3000)))
   ([remap just-one-space] . cycle-spacing)))

;; http://ergoemacs.org/emacs/emacs_align_and_sort.html
(use-package align
  :bind
  (("C-x \\" . align-regexp)))

;; Delete the selection with a key press
(use-package delsel
  :config
  (delete-selection-mode +1))

;; Fix Emacs performance when edit so-long files
(use-package so-long
  :config
  (global-so-long-mode +1))

;; Make word-based commands stop inside symbols
(use-package subword
  :config
  (global-subword-mode +1))

(use-package saveplace
  :config
  (save-place-mode +1))

(use-package elec-pair
  :config
  (electric-pair-mode +1))

(use-package paren
  :config
  (setq show-paren-context-when-offscreen 'overlay)
  (show-paren-mode +1))

(use-package midnight)

(use-package winner
  :config
  (setq winner-boring-buffers '("*Apropos*" "*Buffer List*"
                                "*Completions*" "*Compile-Log*"
                                "*Help*" "*Ibuffer*"
                                "*inferior-lisp*"))
  (winner-mode +1))

(use-package autorevert
  :config
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode +1))

(use-package recentf
  :hook (after-init . recentf-mode)
  :bind (("C-c f f" . recentf-open-files)
         ("C-c f l" . recentf-load-list))
  :custom
  (recentf-max-saved-items 100)
  ;; It can be laggy when cleanup remote files
  (recentf-auto-cleanup 'never)
  :config
  (dolist (regexp '("^/\\(?:ssh\\|su\\|sudo\\)?x?:"
                    "/\\.?TAGS\\'" "/\\.?tags\\'"))
    (add-to-list 'recentf-exclude regexp)))

(use-package whitespace
  :bind ("C-c t w" . whitespace-mode)
  :custom
  ;; Search {zero,full}-width space also
  (whitespace-space-regexp "\\( +\\|　+\\|​+\\)")
  :config
  ;; Show zero-width space
  (add-to-list 'whitespace-display-mappings '(space-mark #x200b [?.])))

(use-package uniquify
  :defer t
  :custom
  ;; Don't muck with special buffers
  (uniquify-ignore-buffers-re "^\\*"))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil))
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(use-package tramp-sh
  :defer t
  :config (cl-pushnew 'tramp-own-remote-path tramp-remote-path))

(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer)
  :custom
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-display-summary nil)
  :commands (ibuffer-switch-to-saved-filter-groups)
  :defines (ibuffer-saved-filter-groups)
  :functions (my--ibuffer-get-major-modes-list
              my--ibuffer-generate-filter-groups-alist
              ibuffer-vc--status-string
              ibuffer-vc--state)
  :hook
  (ibuffer-mode . (lambda ()
                    (ibuffer-switch-to-saved-filter-groups "default")))
  ;; Update filter group when calling `ibuffer'
  (ibuffer . my--ibuffer-generate-filter-groups-by-major-mode)
  :config
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

  (defun my--ibuffer-get-major-modes-list ()
    "Get all major modes based on opened buffers."
    (mapcar
     (lambda (buffer)
       (buffer-local-value 'major-mode (get-buffer buffer)))
     (buffer-list (selected-frame))))

  (defun my--ibuffer-generate-filter-groups-alist (mm-list result-list)
    "Create an alist of filtering groups to switch between."
    (if mm-list
        (let* ((cur-mm (car mm-list))
               (next-res-list-el `(,(capitalize
                                     ;; Trim `-mode' string
                                     (substring (symbol-name cur-mm) 0 -5))
                                   (mode . ,cur-mm))))
          (my--ibuffer-generate-filter-groups-alist
           (cdr mm-list) (cons next-res-list-el result-list)))
      result-list))

  (defun my--ibuffer-generate-filter-groups-by-major-mode ()
    "Generate `ibuffer-saved-filter-groups' by major mode."
    (let* ((ignore-modes '(Buffer-menu-mode
                           compilation-mode
                           minibuffer-inactive-mode
                           ibuffer-mode
                           magit-process-mode
                           messages-buffer-mode
                           fundamental-mode
                           completion-list-mode
                           help-mode
                           Info-mode))
           (groups
            (list
             (cons "default"
                   (my--ibuffer-generate-filter-groups-alist
                    ;; Created by major mode
                    (cl-set-difference
                     (cl-remove-duplicates
                      (my--ibuffer-get-major-modes-list))
                     ignore-modes)
                    ;; Manually created
                    '(("Modified" (predicate buffer-modified-p
                                             (current-buffer)))))))))
      (setq ibuffer-saved-filter-groups groups)
      (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package epg
  :defer t
  :custom (epg-pinentry-mode 'loopback))

(use-package fortune
  :defer t
  :custom
  (fortune-dir "~/fortunes/")
  (fortune-file (expand-file-name "." fortune-dir)))

(use-package calendar
  :defer t
  :custom (calendar-chinese-all-holidays-flag t))

(use-package holidays
  :defer t
  :custom
  (holiday-local-holidays
   `((holiday-fixed 3 8  "Women's Day")
     (holiday-fixed 3 12 "Arbor Day")
     ,@(cl-loop for i from 1 to 3
                collect `(holiday-fixed 5 ,i "International Workers' Day"))
     (holiday-fixed 5 4  "Chinese Youth Day")
     (holiday-fixed 6 1  "Children's Day")
     (holiday-fixed 9 9  "Mourn of Mao's Death")
     (holiday-fixed 9 10 "Teachers' Day")
     ,@(cl-loop for i from 1 to 7
                collect `(holiday-fixed 10 ,i "National Day"))
     (holiday-fixed 12 26 "Mao's Birthday"))))

(use-package ediff
  :defer t
  :custom
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package project
  :defer t
  :config
  (require 'keymap)
  (require 'cl-seq)

  (keymap-substitute project-prefix-map #'project-find-regexp #'consult-ripgrep)
  (cl-nsubstitute-if
   '(consult-ripgrep "Find regexp")
   (pcase-lambda (`(,cmd _)) (eq cmd #'project-find-regexp))
   project-switch-commands)

  (keymap-substitute project-prefix-map #'project-find-file #'consult-fd)
  (cl-nsubstitute-if
   '(consult-fd "Find file")
   (pcase-lambda (`(,cmd _)) (eq cmd #'project-find-file))
   project-switch-commands)

  (keymap-set project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

(use-package tab-bar
  :defer t
  :custom
  ;; Always keep the tab bar hidden
  (tab-bar-show nil)
  (tab-bar-new-tab-choice "*scratch*"))

(use-package dired
  :bind (:map dired-mode-map
              ("," . dired-up-directory)
              ("e" . my-dired-open-externally)
              ("_" . my-dired-cycle-space-underscore-hyphen)
              ("C-c C-e" . my-ediff-files)
              ("C-c C-p" . wdired-change-to-wdired-mode))
  :init
  (defun my-ediff-files ()
    "Run Ediff on the two marked files.

URL `https://oremacs.com/2017/03/18/dired-ediff/'."
    (interactive)
    (let ((files (dired-get-marked-files))
          (wnd (current-window-configuration)))
      (cond
       ((<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "File: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd)))))
       (t
        (user-error "Mark more than 2 files")))))

  (defun my-dired-cycle-space-underscore-hyphen ()
    "Cycle marked files name between space, hyphen and underscore."
    (interactive)
    (mapc (lambda (x)
            (let ((x (file-name-nondirectory x)))
              (cond
               ((string-match " " x)
                (rename-file x (replace-regexp-in-string " " "-" x)))
               ((string-match "-" x)
                (rename-file x (replace-regexp-in-string "-" "_" x)))
               ((string-match "_" x)
                (rename-file x (replace-regexp-in-string "_" " " x))))))
          (dired-get-marked-files))
    (revert-buffer))

  (defun my-dired-open-externally (&optional arg)
    "Open marked or current file in OS's default application."
    (interactive "P")
    (dired-map-over-marks
     (my-open-externally (dired-get-file-for-visit))
     arg))
  :custom
  (dired-listing-switches "-alh")
  ;; Search file name only when focus is over filename
  (dired-isearch-filenames 'dwim)
  ;; Kill current dired buffer when selecting a new directory
  (dired-kill-when-opening-new-dired-buffer t)
  ;; Make dired "guess" target directory
  (dired-dwim-target t))

;;; Mode-line
;; https://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Format.html#Mode-Line-Format

(use-package emacs
  :preface

  (defmacro my-defvar-risky (var value &optional docstring)
    "Define VAR with VALUE and mark it as risky.

Unless the symbol has a non-nil `risky-local-variable'
property, all properties in any strings, as well as all `:eval' and
`:propertize' forms in the value, are ignored."
    (declare (doc-string 3)
             (debug t)
             (indent defun))
    `(progn
       (defvar ,var ,value ,docstring)
       (put ',var 'risky-local-variable t)))

  :custom

  ;; Unify the eol mnemonics for all systems
  (eol-mnemonic-unix ":")
  (eol-mnemonic-mac "/")
  (eol-mnemonic-dos "\\")

  ;; Time format

  (display-time-24hr-format t)
  (display-time-day-and-date t)
  ;; Do NOT display the load average
  (display-time-default-load-average nil)

  ;; ;; Check `format-time-string'
  ;; (display-time-format "%a %b %e")

  :config

  ;; ;; Show date on mode-line
  ;; (display-time-mode +1)

  ;; Make the position number update correctly in all cases
  (line-number-mode +1)
  (column-number-mode +1)

  ;; Human readable representation of file size in mode-line
  (size-indication-mode +1)

  ;; Buffer name
  (my-defvar-risky my--mode-line-buffer-identification
    '(:propertize " %b ")
    "Buffer name, instead of `mode-line-buffer-identification'.")

  ;; Position: line and column
  (my-defvar-risky my--mode-line-position
    '(" %l:%C ")
    "Line and column numbers.")

  ;; File info: percentage, remote, size
  (my-defvar-risky my--mode-line-file-info
    '((:propertize "%p" 'face nil)
      (:propertize "%@" 'face nil)
      (:propertize "%I" 'face nil))
    "File position, remote indicator, and size.")

  ;; Modes: major mode, process, narrow...
  (my-defvar-risky my--mode-line-modes
    (let ((recursive-edit-help-echo
           "Recursive edit, type C-M-c to get out"))
      (list (propertize "%[" 'help-echo recursive-edit-help-echo)
            `(:propertize ("" mode-name)
                          help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                          mouse-face mode-line-highlight
                          local-map ,mode-line-major-mode-keymap)
            '("" mode-line-process)
            (propertize "%]" 'help-echo recursive-edit-help-echo)))
    "Major mode and status indicators.")

  (defvar my-mode-line-format-left
    (list ""
          'mode-line-front-space
          'mode-line-mule-info
          'mode-line-client
          'mode-line-modified
          'mode-line-remote
          'my--mode-line-buffer-identification
          'my--mode-line-position
          'my--mode-line-file-info
          'evil-mode-line-tag
          'mode-line-frame-identification)
    "Left side mode-line.")

  (defvar my-mode-line-format-right
    (list ""
          'mode-line-misc-info
          'my--mode-line-modes
          '(vc-mode vc-mode)
          " ")
    "Right side mode-line.")

  (setq-default mode-line-format
                (list "%e"
                      my-mode-line-format-left
                      '(:eval
                        (let ((mode-line-right-width
                               (string-width
                                (format-mode-line my-mode-line-format-right))))
                          (propertize
                           " "
                           'display
                           `((space :align-to (- right
                                                 ,mode-line-right-width))))))
                      my-mode-line-format-right)))

;;; GUI

(when (display-graphic-p)

;;;; Frame

  (use-package emacs
    :preface

    (defun my-set-window-transparency (value)
      "Set the VALUE of transparency of the frame window."
      (interactive "nSet transparency (0 is transparent - 100 is opaque): ")
      (set-frame-parameter (selected-frame) 'alpha value))

    (defun my-set-line-spacing (space)
      "Set the line SPACE of the current window."
      (interactive "nLine Space: ")
      (setopt line-spacing space))

    :config
    (setq-default frame-title-format "GNU Emacs %@ %b")

    (when (featurep 'ns)
      ;; Make NS behavior the same as other platforms
      (setopt ns-command-modifier 'meta)
      (setopt ns-alternate-modifier 'super)

      (push '(ns-transparent-titlebar . t) default-frame-alist)

      (defun my--set-frame-ns-appearance (frame &rest _)
        "Set ns-appearance frame parameter for FRAME."
        (when (display-graphic-p frame)
          (let ((mode (frame-parameter frame 'background-mode)))
            (modify-frame-parameters frame `((ns-appearance . ,mode))))))

      (defun my--set-all-frames-ns-appearance (&rest _)
        "Set ns-appearance frame parameter for all frames."
        (mapc #'my--set-frame-ns-appearance (frame-list)))

      (add-hook 'after-init-hook #'my--set-all-frames-ns-appearance)
      (add-hook 'after-make-frame-functions #'my--set-frame-ns-appearance)
      (advice-add 'frame-set-background-mode
                  :after #'my--set-frame-ns-appearance))

    (pixel-scroll-precision-mode +1)
    (blink-cursor-mode -1))

;;;; Font

  ;; ;; https://archive.casouri.cat/note/2019/emacs-%E5%AD%97%E4%BD%93%E4%B8%8E%E5%AD%97%E4%BD%93%E9%9B%86/index.html
  ;; ;; http://ergoemacs.org/emacs/emacs_list_and_set_font.html
  ;; ;;
  ;; ;; Emacs use `symbola' (https://dn-works.com/ufas/) as the default
  ;; ;; fallback font. Install to avoid traversing all fonts
  ;; ;;
  ;; ;; NOTE: I am using `my-load-font' to handle this now
  ;; ;;
  ;; ;; Default font
  ;; (set-face-attribute 'default nil :font (font-spec :family "Unifont" :size 16))
  ;; ;;
  ;; ;; East Asia: 你好，こんにちは，안녕하세요。
  ;; ;;
  ;; ;; ¯\_(ツ)_/¯
  ;; (dolist (charset '(han cjk-misc))
  ;;   (set-fontset-font t charset "LXGW WenKai Mono"))
  ;; (set-fontset-font t 'kana "LXGW WenKai Mono")
  ;; (set-fontset-font t 'hangul "LXGW WenKai Mono")

  (use-package emacs
    :preface

    (defcustom my-font-alist
      '(("Maple" "Maple Mono CN" nil 1)
        ("Maple Normal" "Maple Mono Normal CN" nil 1)
        ("霞鹜文楷等宽" "LXGW WenKai Mono" nil 1)
        ("Fira Code" "Fira Code" "LXGW WenKai Mono" 1)
        ("Jetbrains Mono" "Jetbrains Mono" "Maple Mono NF CN" 1)
        ("Unifont" "Unifont" nil 1)
        ("更纱黑体" "Sarasa Gothic SC" nil 1)
        ("等距更纱黑体" "Sarasa Mono SC" nil 1)
        ("霞鹜文楷" "LXGW WenKai" nil 1))
      "An alist of all the fonts you can switch between by `my-load-font'.

Each element is like

    (FONT-NAME . (ASCII-NAME CJK-NAME CJK-SCALE ASCII-SPEC CJK-SPEC))

FONT-NAME is the display name, ASCII-NAME is the ASCII font
family name, CJK-NAME is the CJK font family name, CJK-SCALE is
the CJK font rescale ratio.  ASCII-SPEC and CJK-SPEC are
additional font spec for ASCII and CJK font."
      :type '(repeat (list (string :tag "Font display name")
                           (string :tag "ASCII font name")
                           (choice (string :tag "CJK font name") (const nil))
                           (number :tag "CJK font rescale ratio")
                           (choice (plist :tag "ASCII font spec"
                                          :key-type symbol
                                          :value-type natnum)
                                   (const nil))
                           (choice (plist :tag "CJK font spec"
                                          :key-type symbol
                                          :value-type natnum)
                                   (const nil)))))

    (defcustom my-font-size 13
      "Default font size."
      :type '(natnum :tag "Font size"))

    (defun my--create-fontset (ascii-spec cjk-spec)
      "Create a fontset NAME with ASCII-SPEC and CJK-SPEC font."
      (let* ((font-hash (sxhash (list ascii-spec cjk-spec)))
             ;; If two fontset have the same ASCII spec and different CJK
             ;; spec, the fontset description is the same, we need to
             ;; differentiate between the two, hence the hash
             (fontset-name
              (format "fontset-%s+%x"
                      (downcase (plist-get ascii-spec :family))
                      ;; Don't want negative sign ("-")
                      (abs font-hash)))
             ;; ASCII font
             (fontset
              (create-fontset-from-fontset-spec
               (font-xlfd-name
                (apply #'font-spec :registry fontset-name ascii-spec)))))
        ;; CJK font
        (dolist (charset '(han kana hangul cjk-misc))
          (set-fontset-font fontset charset (apply #'font-spec cjk-spec)))
        fontset))

    (defun my--font-expand-spec (font-spec size)
      "Translate FONT-SPEC and SIZE to (ASCII-SPEC CJK-SPEC).

FONT-SPEC should be a list (ASCII-FAMILY CJK-FAMILY CJK-SCALE
ASCII-SPEC CJK-SPEC), where ASCII-FAMILY is a ASCII font family,
CJK-FAMILY is the CJK font family, and SCJK-SCALE is the scale
factor of CJK font. ASCII-SPEC and CJK-SPEC are extra spec for
ASCII and CJK.

If CJK is nil, the returned CJK-SPEC is nil. If SIZE is nil,
don't add size attributes to the two font spec. If SIZE or
SCJK-SCALE is nil, don't add size attributes to the CJK spec."
      (let* ((ascii-family (nth 0 font-spec))
             (cjk-family   (nth 1 font-spec))
             (cjk-scale    (nth 2 font-spec))
             (ascii-extra-spec
              (and size (append `(:size ,size) (nth 3 font-spec))))
             (cjk-extra-spec
              (and size cjk-scale (append `(:size ,(* cjk-scale size))
                                          (nth 4 font-spec))))
             (ascii-spec (and ascii-family
                              `(:family ,ascii-family ,@ascii-extra-spec)))
             (cjk-spec (and cjk-family
                            `(:family ,cjk-family ,@cjk-extra-spec))))
        (list ascii-spec cjk-spec)))

    (defun my--font-name-to-spec (&optional font-name)
      "Translate FONT-NAME to font-spec.

If FONT-NAME is nil, use the first font in `my-font-alist'."
      (or (alist-get font-name my-font-alist nil nil #'equal)
          (cdar my-font-alist)))

    (defun my--load-font-spec (face font-name size &rest attrs)
      "Load FONT-SPEC for FACE.

FONT-SPEC should be a list (ASCII-FAMILY CJK-FAMILY CJK-SCALE
ASCII-SPEC CJK-SPEC), where ASCII-FAMILY is a ASCII font family,
CJK-FAMILY is the CJK font family, and SCJK-SCALE is the scale
factor of CJK font.  ASCII-SPEC and CJK-SPEC are extra spec for
ASCII and CJK."
      (if (eq face 'default)
          (apply #'my-load-default-font font-name size attrs)
        (let ((fontset
               (apply #'my--create-fontset
                      (my--font-expand-spec
                       (my--font-name-to-spec font-name) size))))
          (apply #'set-face-attribute face nil
                 ;; We must set both `:font' and `fontset' for both ASCII
                 ;; and non-ascii spec to take effect
                 :font fontset
                 :fontset fontset
                 attrs))))

    (defun my-load-font (face font-name size &rest attrs)
      "Load FONT-NAME for FACE with SIZE and ATTRS.

If FONT-NAME is nil, use the first font in `my-font-alist'.
SIZE is the font size in pt.  Add additional face attributes in
ATTRS."
      (interactive
       (list (intern (completing-read "Face: " (face-list) nil t))
             (completing-read "Font: " (mapcar #'car my-font-alist) nil t)
             (read-number "Size: " my-font-size)))
      (let* ((spec (my--font-name-to-spec font-name))
             (fontset (apply #'my--create-fontset
                             (my--font-expand-spec spec size))))
        (if (eq face 'default)
            (apply #'my-load-default-font font-name size attrs)
          (apply #'set-face-attribute face nil
                 :font fontset
                 :fontset fontset
                 attrs))))

    (defun my-load-default-font (font-name size &rest attrs)
      "Load FONT-NAME for default face with SIZE and ATTRS.

More details are inside `my-load-font'."
      ;; We use a separate function for default font because Emacs has a
      ;; bug that prevents us from setting a fontset for the default face
      ;; (although `set-frame-parameter' works). So we just set default
      ;; face with ASCII font and use default fontset for Unicode font
      (interactive
       (list (completing-read "Font: " (mapcar #'car my-font-alist) nil t)
             (read-number "Size: " my-font-size)))
      (let* ((spec (my--font-expand-spec
                    (my--font-name-to-spec font-name)
                    size))
             (ascii (apply #'font-spec (car spec)))
             (cjk (apply #'font-spec (cadr spec))))
        (apply #'set-face-attribute 'default nil :font ascii attrs)
        (set-fontset-font t 'han cjk)
        (set-fontset-font t 'kana cjk)
        (set-fontset-font t 'hangul cjk)
        (set-fontset-font t 'cjk-misc cjk)
        (set-fontset-font t 'symbol cjk nil 'append)))

    :bind

    (("C-c m f" . my-load-default-font)
     ("C-c m F" . my-load-font))

    :config

    (my-load-default-font nil my-font-size)

    ;; Emoji display
    (set-fontset-font t 'emoji
                      (font-spec
                       :family
                       (string-join
                        (list (if (eq system-type 'darwin) "Apple" "Noto")
                              "Color"
                              "Emoji")
                        " "))
                      nil 'prepend)))

;;; Program

(use-package treesit
  :if (treesit-available-p)
  :init
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (c3 "https://github.com/c3lang/tree-sitter-c3")
          (clojure "https://github.com/sogaiu/tree-sitter-clojure")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (dart "https://github.com/UserNobody14/tree-sitter-dart")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (doxygen "https://github.com/tree-sitter-grammars/tree-sitter-doxygen")
          (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
          (heex "https://github.com/phoenixframework/tree-sitter-heex")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (just "https://github.com/IndianBoy42/tree-sitter-just")
          (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
          (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src")
          (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown-inline/src")
          (nix "https://github.com/nix-community/tree-sitter-nix")
          (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" nil "grammars/ocaml/src")
          (ocaml-interface "https://github.com/tree-sitter/tree-sitter-ocaml" nil "grammars/interface/src")
          (odin "https://github.com/tree-sitter-grammars/tree-sitter-odin")
          (php "https://github.com/tree-sitter/tree-sitter-php" nil "php/src")
          (phpdoc "https://github.com/claytonrcarter/tree-sitter-phpdoc")
          (purescript "https://github.com/postsolar/tree-sitter-purescript")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
          (typst "https://github.com/uben0/tree-sitter-typst")
          (vue "https://github.com/tree-sitter-grammars/tree-sitter-vue")
          (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
          (zig "https://github.com/maxxnino/tree-sitter-zig")))
  :custom
  (major-mode-remap-alist
   '((c-mode          . c-ts-mode)
     (c++-mode        . c++-ts-mode)
     (c-or-c++-mode   . c-or-c++-ts-mode)
     (conf-toml-mode  . toml-ts-mode)
     (csharp-mode     . csharp-ts-mode)
     (css-mode        . css-ts-mode)
     (html-mode       . html-ts-mode)
     (java-mode       . java-ts-mode)
     (javascript-mode . js-ts-mode)
     (js-json-mode    . json-ts-mode)
     (js-mode         . js-ts-mode)
     (mhtml-mode      . mhtml-ts-mode)
     (python-mode     . python-ts-mode)
     (ruby-mode       . ruby-ts-mode)
     (sh-mode         . bash-ts-mode)))
  :config
  ;; Add `*-ts-mode' to `auto-mode-alist'
  (dolist (list `((cmake      . (,(rx (or "CMakeLists.txt" ".cmake") eos) . cmake-ts-mode))
                  (dockerfile . (,(rx "Dockerfile" eos) . dockerfile-ts-mode))
                  (elixir     . (,(rx (or ".elixir" (seq ".ex" (opt "s")) "mix.lock") eos) . elixir-ts-mode))
                  (go         . (,(rx ".go" eos) . go-ts-mode))
                  (gomod      . (,(rx "/go.mod" eos) . go-mod-ts-mode))
                  (heex       . (,(rx "." (opt (any "hl")) "eex" eos) . heex-ts-mode))
                  (lua        . (,(rx ".lua" eos) . lua-ts-mode))
                  (tsx        . (,(rx "." (any "jt") "sx" eos) . tsx-ts-mode))
                  (typescript . (,(rx ".ts" eos) . typescript-ts-mode))
                  (yaml       . (,(rx ".y" (opt "a") "ml" eos) . yaml-ts-mode))))
    (let ((parser (car list))
          (alist (cdr list)))
      (when (treesit-ready-p parser 'message)
        (add-to-list 'auto-mode-alist alist)))))

(use-package js
  :mode ("\\.[cm]js\\'" . js-mode)
  :custom (js-indent-level 2))

(use-package lua-ts-mode
  :if (treesit-available-p)
  :defer t
  :custom (lua-ts-indent-offset 3))

(use-package python
  :mode ("\\.[cir]py\\'" . python-mode)
  :custom
  (python-indent-guess-indent-offset nil)
  (python-indent-offset 4))

(use-package tex-mode
  :defer t
  :config
  (setq tex-command "xelatex")
  (add-to-list 'tex-compile-commands '("xelatex %f" t "%r.pdf")))

(use-package eglot
  :bind (("C-c l l" . eglot)
         ("C-c l a" . eglot-code-actions)
         ("C-c l c" . eglot-show-workspace-configuration)
         ("C-c l d" . eglot-find-declaration)
         ("C-c l f" . eglot-format)
         ("C-c l h" . eldoc)
         ("C-c l i" . eglot-find-implementation)
         ("C-c l n" . eglot-rename)
         ("C-c l q" . eglot-shutdown)
         ("C-c l t" . eglot-find-typeDefinition)
         ("C-c l R" . eglot-reconnect)
         ("C-c l Q" . eglot-shutdown-all))
  :config
  ;; Use vtsls instead of ts_ls
  (add-to-list 'eglot-server-programs
               '(((js-mode :language-id "javascript")
                  (js-ts-mode :language-id "javascript")
                  (tsx-ts-mode :language-id "typescriptreact")
                  (typescript-ts-mode :language-id "typescript")
                  (typescript-mode :language-id "typescript"))
                 "vtsls" "--stdio")))

(use-package eglot-booster
  :if (executable-find "emacs-lsp-booster")
  :after eglot
  :custom (eglot-booster-io-only t)
  :config (eglot-booster-mode +1))

(use-package eglot-tempel
  :after (eglot tempel)
  :config (eglot-tempel-mode +1))

(use-package compile
  :bind (("C-c c k" . compile)
         ("C-c c r" . recompile))
  :custom
  (compilation-ask-about-save nil)
  (compilation-always-kill t)
  (compilation-scroll-output 'first-error)
  :hook
  (compilation-filter . ansi-color-compilation-filter))

(use-package flymake
  :bind (("C-c ! b" . flymake-show-buffer-diagnostics)
         ("C-c ! p" . flymake-show-project-diagnostics)))

(use-package flyspell
  :defer t
  :config
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell"
          ispell-extra-args '("--sug-mode=ultra"
                              "--lang=en_US"
                              "--camel-case"))))

(use-package eldoc-box
  :if (display-graphic-p)
  :hook ((eldoc-mode eglot-managed-mode) . eldoc-box-hover-mode)
  :custom
  (eldoc-box-only-multi-line t)
  (eldoc-box-clear-with-C-g t)
  (eldoc-box-enable-frame-map t)
  :bind (("C-c h h" . eldoc-box-help-at-point)
         (:map eldoc-box-frame-map
               ("C-M-n" . eldoc-box-scroll-up)
               ("C-M-p" . eldoc-box-scroll-down)
               ("C-M-a" . eldoc-box-beginning)
               ("C-M-e" . eldoc-box-end)))
  :config
  (add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t)
  (setq eldoc-doc-buffer-separator
        (concat "\n"
                (propertize "-"
                            'display '(space :align-to right)
                            'face '(:strike-through t)
                            'font-lock-face '(:strike-through t))
                "\n")))

(use-package citre
  :bind (("C-c c a" . citre-ace-peek)
         ("C-c c e" . citre-edit-tags-file-recipe)
         ("C-c c h" . citre-peek)
         ("C-c c t" . citre-update-this-tags-file)
         ("C-c c j" . citre-jump)
         ("C-c c J" . citre-jump-back))
  :custom
  (citre-auto-enable-citre-mode-modes '(prog-mode))
  (citre-default-create-tags-file-location 'global-cache)
  :config
  ;; Add Elisp to the backend lists
  (citre-register-backend 'elisp
                          (citre-xref-backend-to-citre-backend
                           'elisp
                           (lambda () (derived-mode-p 'emacs-lisp-mode))))
  (add-to-list 'citre-find-definition-backends 'elisp)
  (add-to-list 'citre-find-reference-backends 'elisp))

(use-package apheleia
  :bind (("C-c c f" . apheleia-format-buffer)
         ("C-c c F" . apheleia-goto-error))
  :config
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff))
  (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
  (add-to-list 'apheleia-mode-alist '(vue-ts-mode . prettier))

  (setf (alist-get 'rustfmt apheleia-formatters)
        '("rustfmt" "--edition" "2024" "--quiet" "--emit" "stdout"))
  (setf (alist-get 'google-java-format apheleia-formatters)
        '("google-java-format" "--aosp" "-")))

(use-package elisp-mode
  :preface
  (defun my-endless-sharp ()
    "Insert #\\=' unless in a string or comment.

URL `https://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html'."
    (interactive)
    (call-interactively #'self-insert-command)
    (let ((ppss (syntax-ppss)))
      (unless (or (elt ppss 3)
                  (elt ppss 4)
                  (eq (char-after) ?\'))
        (insert "'"))))

  (defun my-eval-print-last-sexp (&optional arg)
    "Evaluate sexp before point, insert output below following an arrow.
With a `\\[universal-argument]' prefix argument ARG, delete the
sexp before point and insert output into current position."
    (interactive "P")
    (let ((value (eval (elisp--preceding-sexp))))
      (save-excursion
        (cond
         ((not arg)
          (newline-and-indent)
          (if (and (stringp value) (string-match-p "\n" value))
              ;; If return value is a multiline string
              (insert (format
                       ";; =>\n;; %S"
                       (replace-regexp-in-string "\n" "\n;; " value)))
            (insert (format "%s%S" ";; => " value))))
         ((equal arg '(4))
          (backward-kill-sexp)
          (insert (format "%S" value)))))))
  :bind ((:map emacs-lisp-mode-map
               ("#" . my-endless-sharp)
               ("C-c C-p" . my-eval-print-last-sexp))
         (:map lisp-interaction-mode-map
               ("C-c C-p" . my-eval-print-last-sexp))))

(use-package sly
  :preface
  (define-advice sly-mrepl (:override (&rest _) last)
    "Switch to the last Lisp/Sly-Mrepl buffer."
    (interactive)
    (if (derived-mode-p 'sly-mrepl-mode)
        (if-let* ((buf (seq-find (lambda (b)
                                   (with-current-buffer b
                                     (derived-mode-p 'lisp-mode)))
                                 (buffer-list))))
            (if-let* ((win (get-buffer-window buf)))
                (select-window win)
              (pop-to-buffer buf))
          (user-error "No Lisp buffer found"))
      (if-let* ((buf (sly-mrepl--find-create (sly-current-connection))))
          (if-let* ((win (get-buffer-window buf)))
              (select-window win)
            (pop-to-buffer buf))
        (user-error "No Sly-Mrepl buffer found"))))
  :bind ((:map sly-mode-map
               ("C-c C-a C-c" . sly-asdf-compile-system)
               ("C-c C-a C-l" . sly-asdf-load-system)
               ("C-c C-a C-r" . sly-asdf-reload-system)
               ("C-c C-a C-t" . sly-asdf-test-system)
               ("C-c C-v C-i" . sly-inspect)
               ("C-c C-v C-d" . sly-inspect-definition)
               ("C-c C-x C-c" . sly-connect)
               ("C-c C-x C-q" . sly-disconnect)
               ("C-c C-x C-j" . sly))
         (:map sly-doc-map
               ("C-l" . sly-documentation)))
  :custom (inferior-lisp-program "sbcl"))

(use-package sly-asdf :after sly)
(use-package sly-quicklisp :after sly)

(use-package racket-mode
  :bind (:map racket-mode-map
              ("C-c C-x C-j" . racket-run)
              ("C-c C-x C-x" . racket-xp-mode)
              ("C-c C-x C-e" . racket-eval-last-sexp)))

(use-package clojure-mode
  :mode
  ("\\.\\(cljd?\\|edn\\)\\'" . clojure-mode)
  ("\\.cljs\\'" . clojurescript-mode))

(use-package clojure-ts-mode
  :if (treesit-available-p)
  :mode
  ("\\.\\(clj\\|edn\\)\\'" . clojure-ts-mode)
  ("\\.cljs\\'" . clojure-ts-clojurescript-mode)
  ("\\.cljd\\'" . clojure-ts-clojuredart-mode))

(use-package cider
  :bind ((:map cider-eval-commands-map
               ("C-p" . cider-inspect-last-result)
               ("C-i" . cider-inspect))
         (:map cider-start-map
               ("r" . cider-restart)
               ("C-r" . cider-restart)))
  :custom
  ;; Require Java >= 21
  (cider-enable-nrepl-jvmti-agent t)
  :config
  ;; https://github.com/clojure-emacs/cider/issues/3588
  (when (and (string= "powershell" cider-clojure-cli-command)
             (executable-find "pwsh"))
    (setq cider-clojure-cli-command "pwsh")))

(use-package fennel-mode
  :mode ("\\.fnl\\'" . fennel-mode))

(use-package c3-ts-mode
  :if (treesit-available-p)
  :mode "\\.c3\\'")

(use-package dart-ts-mode
  :if (treesit-available-p)
  :mode "\\.dart\\'")

(use-package fsharp-mode
  :bind (:map fsharp-mode-map
              ("C-c C-x C-j" . run-fsharp)))

(use-package eglot-fsharp :after (eglot fsharp-mode))

(use-package haskell-ts-mode
  :if (treesit-available-p)
  :bind (:map haskell-ts-mode-map
              ("C-c C-x C-j" . run-haskell))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(haskell-ts-mode
                   . ("haskell-language-server-wrapper" "--lsp"))))
  :mode "\\.hs\\'")

(use-package purescript-mode
  :defer t)

(use-package just-ts-mode
  :if (treesit-available-p)
  :mode "\\.[Jj]ust\\(file\\)?\\'")

(use-package nix-ts-mode
  :if (treesit-available-p)
  :mode "\\.nix\\'")

(use-package neocaml
  :if (treesit-available-p)
  :hook (neocaml-mode . neocaml-repl-minor-mode)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((neocaml-mode :language-id "ocaml") . ("ocamllsp"))))
  :mode
  (("\\.mli\\'" . neocamli-mode)
   ("\\.ml\\'" . neocaml-mode)))

(use-package ocaml-eglot
  :after (eglot neocaml)
  :config (ocaml-eglot +1))

(use-package odin-ts-mode
  :if (treesit-available-p)
  :mode "\\.odin\\'")

(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-c C-c" . rust-compile)
              ("C-c C-c C-d" . rust-dbg-wrap-or-unwrap)
              ("C-c C-c C-m" . rust-toggle-mutability)
              ;; Unbind `rust-dbg-wrap-or-unwrap' for doc
              ("C-c C-d" . nil)
              ("C-c C-d C-d" . my-rust-doc)
              ("C-c C-d C-o" . my-rust-doc-open)
              ("C-c C-p C-b" . rust-playpen-buffer)
              ("C-c C-p C-r" . rust-playpen-region)
              ("C-c C-r C-c" . rust-compile-release)
              ("C-c C-r C-r" . rust-run-release))
  ;; :custom (rust-mode-treesitter-derive t)
  :config
  (defun my-rust-doc ()
    "Build documentation using `cargo doc'."
    (interactive)
    (rust--compile nil "%s doc" rust-cargo-bin))

  (defun my-rust-doc-open ()
    "Build and open documentation using `cargo doc'."
    (interactive)
    (rust--compile nil "%s doc --open" rust-cargo-bin)))

(use-package vue-ts-mode
  :if (treesit-available-p)
  :config
  (with-eval-after-load 'eglot
    ;; Eglot with vuels
    (defcustom my--eglot-vuels-path "/path/to/@vue/language-server"
      "Path to vue-language-server."
      :type '(directory :tag "Path to vuels")
      :group 'eglot)

    (add-to-list 'eglot-server-programs
                 '(vue-ts-mode . (eglot-vtsls "vtsls" "--stdio")))

    (defclass eglot-vtsls (eglot-lsp-server) ()
      :documentation "vtsls-language-server")

    (cl-defmethod eglot-initialization-options ((server eglot-vtsls))
      "Pass through required cquery initialization options"
      `(:vtsls
        (:tsserver
         (:globalPlugins
          [( :name "@vue/typescript-plugin"
             :location ,my--eglot-vuels-path
             :languages ["vue"]
             :configNamespace "typescript")])))))
  :mode "\\.[nu]?vue\\'")

(use-package zig-ts-mode
  :if (treesit-available-p)
  :mode "\\.zig\\'")

;;; Version control

(use-package vc
  :defer t
  :config
  ;; Visit version controlled symlink without asking
  (setq vc-follow-symlinks t))

(use-package magit
  :bind (("C-x g"   . magit-status)
         ("C-c v g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :config
  ;; Add module section into the status buffer
  (magit-add-section-hook 'magit-status-sections-hook
                          #'magit-insert-modules
                          #'magit-insert-stashes #'append))

(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (after-init . diff-hl-flydiff-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-margin-symbols-alist '((insert . "+") (delete . "-")
                                  (change . "=") (ignored . "!")
                                  (unknown . "?"))))

(use-package git-modes
  :mode ("/\\.dockerignore\\'" . gitignore-mode))

(use-package git-link
  :custom (git-link-use-commit t)
  :bind (("C-c v l" . git-link)
         ("C-c v c" . git-link-commit)
         ("C-c v d" . git-link-dispatch)
         ("C-c v h" . git-link-homepage)))

(use-package git-timemachine
  :bind ("C-c v t" . git-timemachine))

;;; Better edit

(use-package vertico
  :custom
  (vertico-cycle t)
  :hook
  (after-init . vertico-mode))

(use-package vertico-sort
  :after vertico
  :custom
  (vertico-sort-function 'vertico-sort-history-length-alpha))

;; Persist history over Emacs restarts for Vertico
(use-package savehist
  :hook (after-init . savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers
  (enable-recursive-minibuffers t)
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the
  ;; current mode.  Vertico commands are hidden in normal
  ;; buffers. This setting is useful beyond Vertico
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  (define-advice completing-read-multiple (:filter-args (args) crm-indicator)
    "Add prompt indicator to `completing-read-multiple'."
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  ;; (corfu-separator ?\s)          ; orderless field separator
  ;; (corfu-quit-at-boundary nil)   ; never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ; never quit, even if there is no match
  ;; (corfu-preview-current nil)    ; disable current candidate preview
  ;; (corfu-preselect 'prompt)      ; disable candidate preselection
  ;; (corfu-on-exact-match nil)     ; configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ; use scroll margin
  :hook
  (after-init . global-corfu-mode))

(use-package corfu-popupinfo
  :if (display-graphic-p)
  :hook (corfu-mode . corfu-popupinfo-mode)
  :bind (:map corfu-map
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-p" . corfu-popupinfo-scroll-down)
              ("M-a" . corfu-popupinfo-beginning)
              ("M-e" . corfu-popupinfo-end)
              ("M-l" . corfu-popupinfo-location)
              ("M-d" . corfu-popupinfo-documentation)
              ("M-t" . corfu-popupinfo-toggle)))

(use-package corfu-info
  :unless (display-graphic-p)
  :after corfu
  :bind (:map corfu-map
              ("M-l" . corfu-info-location)
              ("M-d" . corfu-info-documentation)))

;; Use Dabbrev with Corfu
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key
  ;; `completion-at-point' is often bound to M-TAB
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an
  ;; alternative, try `cape-dict'
  (text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to
  ;; the current mode.  Corfu commands are hidden, since they are not
  ;; used via M-x. This setting is useful beyond Corfu
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic
  ;; key.  Press C-c k ? to for help
  :bind ("C-c k" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually
  ;; :bind (("C-c k d" . cape-dabbrev)
  ;;        ("C-c k h" . cape-history)
  ;;        ("C-c k f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of
  ;; `completion-at-point-functions' which is used by
  ;; `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of
  ;; buffer-local completion functions takes precedence over the
  ;; global list
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...

  (defun my--cape-dict-file ()
    "Return `cape-dict-file'."
    (let ((file "/usr/share/dict/words"))
      (if (eq system-type 'windows-nt)
          "~/.dict"
        (list file "~/.dict"))))

  :custom
  (cape-dict-file #'my--cape-dict-file))

(use-package zh-lib
  :defer t
  :custom (zh-lib-scheme 'simplified-traditional-quanpin-all))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package embark
  :bind (("M-A" . embark-act)
         ("M-E" . embark-export)
         ("M-D" . embark-dwim)
         ([remap describe-bindings] . embark-bindings)
         (:map minibuffer-local-map
               ("M-a" . embark-act)
               ("M-e" . embark-export)
               ("M-d" . embark-dwim)
               ("M-." . my-embark-preview)
               ("C-c C-a" . embark-act)
               ("C-c C-o" . embark-export)
               ("C-c C-c" . embark-dwim)))
  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  :config
  (defun my-embark-preview ()
    "Previews candidate in vertico buffer, unless it's a consult command"
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim)))))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package workspaces
  :bind (("C-c C-w" . workspaces-prefix-map)
         ("C-c w s" . workspaces-switch)
         ("C-c w l" . workspaces-switch)
         ("C-c w o" . workspaces-open))
  :hook (after-init . workspaces-mode)
  :config
  ;; Integrate workspace buffers into `consult-buffer'
  (with-eval-after-load 'consult
    (defvar consult--source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'workspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))
      "Workspace buffer candidate source for `consult-buffer'.")

    (defun my--consult-workspaces ()
      "Isolate workspace buffers when using workspaces."
      (if workspaces-mode
          (add-to-list 'consult-buffer-sources 'consult--source-workspace)
        ;; Reset `consult-buffer' to show all buffers
        (setq consult-buffer-sources
              (remove #'consult--source-workspace consult-buffer-sources))))

    (my--consult-workspaces)
    (add-hook 'workspaces-mode-hook #'my--consult-workspaces)))

(use-package ace-window
  :bind ([remap other-window] . ace-window)
  :config
  ;; Inherits from `avy'
  (with-eval-after-load 'avy
    (setq aw-keys avy-keys
          aw-background avy-background)))

(use-package winum
  :hook (after-init . winum-mode)
  :custom
  (winum-format "%s ")
  (winum-mode-line-position 0))

;; Jump between texts
;; https://emacsredux.com/blog/2015/07/19/ace-jump-mode-is-dead-long-live-avy
(use-package avy
  :preface

  (defun my-avy-copy-thing-at-point ()
    "Copy thing at point using `avy'."
    (interactive)
    (save-excursion
      (avy-goto-word-or-subword-1)
      (kill-new (thing-at-point
                 (cl-case (read-char "w: word, s: symbol, l: list, u: url")
                   (?w 'word)
                   (?s 'symbol)
                   (?l 'list)
                   (?u 'url))))))

  :bind (("C-c g 2" . avy-goto-char-2)
         ("C-c g c" . avy-goto-char)
         ("C-c g e" . avy-goto-end-of-line)
         ("C-c g g" . avy-goto-char-timer)
         ("C-c g i" . avy-goto-char-in-line)
         ("C-c g j" . avy-goto-line-below)
         ("C-c g k" . avy-goto-line-above)
         ("C-c g l" . avy-goto-line)
         ("C-c g w" . avy-goto-word-or-subword-1)
         ("C-c m c" . my-avy-copy-thing-at-point)
         (:map dired-mode-map
               :package dired-mode
               (";" . avy-goto-char-2))
         (:map isearch-mode-map
               ("C-a" . avy-isearch)
               ("C-'" . avy-isearch)))
  :custom (avy-style 'at-full))

(use-package avy-zh
  :after avy
  :config (avy-zh-mode +1))

(use-package expreg
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)
         ("C-c e ;" . expreg-expand)
         ("C-c e '" . expreg-contract)))

(use-package vundo
  :bind ("C-c e u" . vundo))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  :config
  (define-advice orderless-regexp (:filter-args (str) enhance)
    "Enhance `orderless-regexp' when searching STR."
    (require 'zh-lib)
    (setf (car str) (zh-lib-build-regexp-string (car str)))
    str))

(use-package consult
  :preface

  (defcustom my-consult-zh-prefix ?:
    "The prefix character when using consult to search Zhongwen."
    :group 'convenience
    :type 'character)

  :init

  ;; Optionally tweak the register preview window
  ;; This adds thin lines, sorting and hides the mode line of the window
  (advice-add 'register-preview :override #'consult-register-window)

  :bind

  (;; Remap bindings
   ([remap bookmark-jump] . consult-bookmark)
   ([remap goto-line] . consult-goto-line)
   ([remap imenu] . consult-imenu)
   ([remap load-theme] . consult-theme)
   ([remap locate] . consult-locate)
   ([remap man] . consult-man)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap recentf-open-files] . consult-recent-file)
   ([remap repeat-complex-command] . consult-complex-command)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap yank-pop] . consult-yank-pop)
   ;; Register access
   ([remap abbrev-prefix-mark] . consult-register-store)
   ("M-#" . consult-register-load)
   ("C-M-#" . consult-register)
   ;; C-c bindings in `mode-specific-map'
   ("C-c M-x" . consult-mode-command)
   ("C-c s I" . consult-imenu-multi)
   ("C-c s f" . consult-fd)
   ("C-c s F" . consult-find)
   ("C-c s g" . consult-ripgrep)
   ("C-c s G" . consult-grep)
   ("C-c s l" . consult-line)
   ("C-c s L" . consult-locate)
   ("C-c s m" . consult-line-multi)
   ("C-c s k" . consult-keep-lines)
   ("C-c s u" . consult-focus-lines)
   ("C-c s o" . consult-outline)
   ("C-c s v" . consult-git-grep)
   ;; M-g bindings in `goto-map'
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ;; M-s bindings in `search-map'
   ("M-s d" . consult-fd)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Other bindings
   ("M-L" . consult-line)
   (:map minibuffer-local-map
         ("M-h" . consult-history)
         ([remap next-matching-history-element] . consult-history)
         ([remap previous-matching-history-element] . consult-history))
   (:map isearch-mode-map
         ("M-h" . consult-isearch-history)
         ("M-l" . consult-line)
         ("M-m" . consult-line-multi)
         ([remap isearch-edit-string] . consult-isearch-history)))

  :custom

  ;; Optionally configure the narrowing key
  ;; Both < and C-+ work reasonably well
  (consult-narrow-key "<")
  ;; Optionally configure the register formatting. This improves the
  ;; register preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)

  :config

  (define-advice consult--default-regexp-compiler (:override (input type ignore-case) zh)
    "Compile the INPUT string to a list of regular expressions.

The function should return a pair, the list of regular expressions and a
highlight function. The highlight function should take a single
argument, the string to highlight given the INPUT. TYPE is the desired
type of regular expression, which can be `basic', `extended', `emacs' or
`pcre'. If IGNORE-CASE is non-nil return a highlight function which
matches case insensitively."
    (require 'zh-lib)
    (setq input (consult--split-escaped
                 (if (char-equal my-consult-zh-prefix (string-to-char input))
                     ;; Detect the first entered character. If it
                     ;; matches `my-consult-zh-prefix', convert the
                     ;; subsequent characters into Zhongwen regexp
                     (zh-lib-build-regexp-string (substring input 1))
                   input)))
    (cons (mapcar (lambda (x) (consult--convert-regexp x type)) input)
          (when-let* ((regexps (seq-filter #'consult--valid-regexp-p input)))
            (apply-partially #'consult--highlight-regexps regexps ignore-case))))

  (when (eq system-type 'windows-nt)
    (define-advice consult-find (:override (&optional dir initial) win)
      "Use `consult-find' on Windows.

URL `https://github.com/minad/consult/issues/475'."
      (pcase-let* ((w32-quote-process-args ?\\) ; or (w32-quote-process-args ?*)
                   (consult-find-args (string-join
                                       (push find-program (cdr (string-split consult-find-args)))
                                       " "))
                   (`(,prompt ,paths ,dir) (consult--directory-prompt "Find" dir))
                   (default-directory dir)
                   (builder (consult--find-make-builder paths)))
        (find-file (consult--find prompt builder initial)))))

  (consult-customize
   consult-theme :preview-key '(:debounce 0.5 any)))

(use-package embark-consult
  :after (embark consult))

(use-package hl-todo
  :hook (after-init . global-hl-todo-mode)
  :bind (:map hl-todo-mode-map
              ("M-s M-o" . hl-todo-occur))
  :custom
  (hl-todo-highlight-punctuation ":"))

(use-package separedit
  :preface

  (defun my-eval-last-sexp-in-comment ()
    "Eval last sexp in comment by using `separedit'."
    (interactive)
    (require 'separedit)
    (let ((separedit-default-mode 'emacs-lisp-mode)
          (separedit-inhibit-edit-window-p t))
      (with-current-buffer (separedit)
        (unwind-protect (call-interactively #'eval-last-sexp)
          (separedit-abort)))))

  :custom

  (separedit-remove-trailing-spaces-in-comment t)
  (separedit-preserve-string-indentation t)

  :bind ("C-c e e" . separedit))

(use-package evil
  :preface

  (defmacro my--evil-adjust-major-mode-keymap (mode &optional replace)
    "Use MODE\\='s keymap in `evil-normal-state' after MODE loaded.

MODE's feature might be provided as REPLACE, in that situation, use
REPLACE instead.  URL `https://github.com/emacs-evil/evil/issues/511'."
    `(with-eval-after-load (quote ,(if replace replace mode))
       (evil-make-overriding-map ,(intern (concat mode "-mode-map")) 'normal)
       (add-hook (quote ,(intern (concat mode "-mode-hook")))
                 #'evil-normalize-keymaps)))

  :hook

  (after-init . evil-mode)

  :init

  ;; https://github.com/emacs-evil/evil/issues/1486#issuecomment-876371225
  (setopt evil-disable-insert-state-bindings t)

  :custom

  (evil-want-C-i-jump nil)
  ;; Use Emacs keys in INSERT state
  (evil-disable-insert-state-bindings t)
  (evil-ex-interactive-search-highlight 'selected-window)
  ;; Move back the cursor one position when exiting insert mode
  (evil-move-cursor-back t)
  ;; Make cursor move as Emacs
  (evil-move-beyond-eol t)
  ;; Make evil search like vim
  (evil-ex-search-vim-style-regexp t)
  ;; Use native undo commands
  (evil-undo-system 'undo-redo)

  :config

  ;; Make evil-search behave more like VIM
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Leader key
  (let ((state '(normal visual motion)))
    ;; SPACE as leader
    (evil-set-leader state (kbd "SPC"))
    ;; Comma as localleader
    (evil-set-leader state (kbd ",") t))

  (defun evil-unimpaired-insert-newline-above (count)
    "Insert COUNT blank line(s) above current line."
    (interactive "p")
    (save-excursion (dotimes (_ count) (evil-insert-newline-above)))
    (when (bolp) (forward-char count)))

  (keymap-set evil-normal-state-map "[ SPC" #'evil-unimpaired-insert-newline-above)

  (defun evil-unimpaired-insert-newline-below (count)
    "Insert COUNT blank line(s) below current line."
    (interactive "p")
    (save-excursion (dotimes (_ count) (evil-insert-newline-below))))

  (keymap-set evil-normal-state-map "] SPC" #'evil-unimpaired-insert-newline-below)

  (define-advice keyboard-quit (:before () evil-ex-nohighlight)
    "Disable evil ex search buffer highlight."
    (when (evil-ex-hl-active-p 'evil-ex-search)
      (evil-ex-nohighlight)))

  (defun my--evil-paren-range (count beg end type inclusive)
    "Get minimum range of paren text object.

COUNT, BEG, END, TYPE is used to identify the text object.
If INCLUSIVE is t, the text object is inclusive.

URL `http://blog.binchen.org/posts/code-faster-by-extending-emacs-evil-text-object'."
    (let ((parens '("()" "[]" "{}" "<>" "\"\"" "''" "``"
                    "（）" "《》" "「」" "『』" "【】" "〖〗"
                    "“”" "‘’" "［］" "〔〕" "｛｝"))
          (pos (point))
          range found-range)
      (dolist (paren parens)
        (condition-case _
            (let ((char1 (aref paren 0))
                  (char2 (aref paren 1)))
              (setq range (if (eq char1 char2)
                              (evil-select-quote char1
                                                 beg end
                                                 type count inclusive)
                            (evil-select-paren char1 char2
                                               beg end
                                               type count inclusive))))
          (error nil))
        (when (and range (<= (nth 0 range) pos) (< pos (nth 1 range)))
          (cond
           (found-range
            (when (< (- (nth 1 range) (nth 0 range))
                     (- (nth 1 found-range) (nth 0 found-range)))
              (setf (nth 0 found-range) (nth 0 range))
              (setf (nth 1 found-range) (nth 1 range))))
           (t
            (setq found-range range)))))
      found-range))

  (evil-define-text-object my--evil-a-paren (count &optional beg end type)
    "Select a text object."
    :extend-selection t
    (my--evil-paren-range count beg end type t))

  (evil-define-text-object my--evil-inner-paren (count &optional beg end type)
    "Select inner text object."
    :extend-selection nil
    (my--evil-paren-range count beg end type nil))

  (keymap-set evil-outer-text-objects-map "a" #'my--evil-a-paren)
  (keymap-set evil-inner-text-objects-map "a" #'my--evil-inner-paren)

  (evil-define-key 'normal org-mode-map
    "gh" #'outline-up-heading
    "gn" #'outline-next-visible-heading
    "gp" #'outline-previous-visible-heading
    "$"  #'org-end-of-line
    "^"  #'org-beginning-of-line
    "<"  #'org-promote-subtree
    ">"  #'org-demote-subtree
    (kbd "TAB") #'org-cycle)

  (evil-define-key 'normal markdown-mode-map
    "gh" #'outline-up-heading
    "gn" #'outline-next-visible-heading
    "gp" #'outline-previous-visible-heading
    "<"  #'markdown-promote
    ">"  #'markdown-demote
    (kbd "TAB") #'markdown-cycle)

  (dolist (b '(;; Special buffers
               ("\\*.*\\*"           . emacs)
               ;; Magit related
               (".*MSG.*"            . emacs)
               ;; Yasnippet
               ("+new-snippet+"      . emacs)
               ;; Evil-ex
               ("\\*Command Line\\*" . normal)
               ("\\*scratch\\*"      . normal)))
    (add-to-list 'evil-buffer-regexps b))

  (setq evil-emacs-state-modes
        (append
         '(calender-mode dired-mode erc-mode image-mode)
         evil-emacs-state-modes))

  (my--evil-adjust-major-mode-keymap "git-timemachine")
  (my--evil-adjust-major-mode-keymap "view")

  :bind

  (
;;;; Evil leader
   ("<leader><SPC>" . execute-extended-command)
   ("<leader>;" . eval-expression)
;;;; Evil leader code
   ("<leader>cc" . compile)
   ("<leader>cf" . apheleia-format-buffer)
   ("<leader>cF" . apheleia-goto-error)
   ("<leader>cp" . project-compile)
   ("<leader>cr" . recompile)
;;;;; Evil leader file
   ("<leader>ff" . find-file)
   ("<leader>fc" . my-copy-file-name)
   ("<leader>fd" . my-delete-this-file)
   ("<leader>fo" . my-open-file-externally)
   ("<leader>fr" . my-rename-this-file)
   ("<leader>f/" . find-file-other-window)
;;;;; Evil leader git
   ("<leader>gd" . magit-dispatch)
   ("<leader>gg" . magit-status)
   ("<leader>gf" . magit-file-dispatch)
   ("<leader>gs" . consult-git-grep)
;;;;; Evil leader lsp
   ("<leader>ll" . eglot)
   ("<leader>la" . eglot-code-actions)
   ("<leader>lc" . eglot-show-workspace-configuration)
   ("<leader>ld" . eglot-find-declaration)
   ("<leader>lf" . eglot-format)
   ("<leader>lh" . eldoc)
   ("<leader>li" . eglot-find-implementation)
   ("<leader>ln" . eglot-rename)
   ("<leader>lq" . eglot-shutdown)
   ("<leader>lt" . eglot-find-typeDefinition)
   ("<leader>lR" . eglot-reconnect)
   ("<leader>lQ" . eglot-shutdown-all)
;;;;; Evil leader my custom
   ("<leader>mc" . my-avy-copy-thing-at-point)
   ("<leader>mD" . my-delete-visual-blank-lines)
   ("<leader>md" . my-delete-blank-lines)
   ("<leader>me" . my-open-externally)
   ("<leader>mF" . my-load-font)
   ("<leader>mf" . my-load-default-font)
   ("<leader>mo" . my-occur-dwim)
   ("<leader>mp" . my-pangu-spacing-current-buffer)
   ("<leader>mT" . my-load-default-theme)
   ("<leader>mt" . my-load-theme)
   ("<leader>mx" . execute-extended-command)
;;;;; Evil leader org
   ("<leader>oa" . org-agenda)
   ("<leader>ob" . org-switchb)
   ("<leader>oc" . org-capture)
   ("<leader>ot" . org-toggle-link-display)
;;;;; Evil leader search
   ("<leader>sf" . consult-fd)
   ("<leader>sF" . consult-find)
   ("<leader>sg" . consult-ripgrep)
   ("<leader>sG" . consult-grep)
   ("<leader>sl" . consult-line)
   ("<leader>sL" . consult-locate)
   ("<leader>sm" . consult-line-multi)
   ("<leader>si" . imenu)
   ("<leader>sI" . consult-imenu-multi)
   ("<leader>sk" . consult-keep-lines)
   ("<leader>su" . consult-focus-lines)
   ("<leader>so" . consult-outline)
   ("<leader>ss" . consult-line)
   ("<leader>sv" . consult-git-grep)
;;;;; Evil leader toggle
   ("<leader>tA"  . abbrev-mode)
   ("<leader>ta"  . auto-fill-mode)
   ("<leader>tD"  . darkroom-mode)
   ("<leader>td"  . darkroom-tentative-mode)
   ("<leader>tff" . toggle-frame-fullscreen)
   ("<leader>tfm" . toggle-frame-maximized)
   ("<leader>tg"  . glasses-mode)
   ("<leader>th"  . global-hl-line-mode)
   ("<leader>ti"  . display-fill-column-indicator-mode)
   ("<leader>tj"  . toggle-truncate-lines)
   ("<leader>tk"  . visual-line-mode)
   ("<leader>tl"  . display-line-numbers-mode)
   ("<leader>ts"  . subword-mode)
   ("<leader>tt"  . load-theme)
   ("<leader>tv"  . view-mode)
   ("<leader>tw"  . whitespace-mode)
;;;; Evil localleader
   ("<localleader>,"  . execute-extended-command)
   ("<localleader>."  . evil-ex)
   ("<localleader>;"  . eval-expression)
   ("<localleader>w"  . save-buffer)
   ("<localleader>Q"  . save-buffers-kill-terminal)
   ("<localleader>aa" . avy-goto-char-2)
   ("<localleader>ac" . avy-goto-char-timer)
   ("<localleader>ae" . avy-goto-end-of-line)
   ("<localleader>af" . beginning-of-defun)
   ("<localleader>ag" . avy-goto-line)
   ("<localleader>aj" . avy-goto-line-below)
   ("<localleader>ak" . avy-goto-line-above)
   ("<localleader>ar" . align-regexp)
   ("<localleader>aw" . avy-goto-word-or-subword-1)
   ("<localleader>bb" . (lambda () (interactive) (switch-to-buffer nil)))
   ("<localleader>cc" . evilnc-comment-or-uncomment-lines)
   ("<localleader>dd" . pwd)
   ("<localleader>dj" . dired-jump)
   ("<localleader>dp" . project-dired)
   ("<localleader>ee" . eval-expression)
   ("<localleader>el" . eval-last-sexp)
   ("<localleader>ff" . consult-fd)
   ("<localleader>gg" . consult-ripgrep)
   ("<localleader>hh" . mark-whole-buffer)
   ("<localleader>kk" . my-kill-other-buffers-without-special-ones)
   ("<localleader>sc" . shell-command)
   ("<localleader>ss" . consult-line)
   ("<localleader>si" . imenu)
   ("<localleader>xx" . flymake-show-buffer-diagnostics)
   ("<localleader>xp" . flymake-show-project-diagnostics)
;;;;; Evil localleader window
   ("<localleader>0"  . winum-select-window-0-or-10)
   ("<localleader>1"  . winum-select-window-1)
   ("<localleader>2"  . winum-select-window-2)
   ("<localleader>3"  . winum-select-window-3)
   ("<localleader>4"  . winum-select-window-4)
   ("<localleader>5"  . winum-select-window-5)
   ("<localleader>6"  . winum-select-window-6)
   ("<localleader>7"  . winum-select-window-7)
   ("<localleader>8"  . winum-select-window-8)
   ("<localleader>9"  . winum-select-window-9)
   ("<localleader>oo" . my-toggle-full-window)
   ("<localleader>sa" . split-window-vertically)
   ("<localleader>sd" . split-window-horizontally)
   ("<localleader>sh" . split-window-below)
   ("<localleader>sq" . delete-window)
   ("<localleader>sv" . split-window-right)
   ("<localleader>rr" . my-rotate-windows)
   ("<localleader>tt" . my-toggle-two-split-window)
;;;; Evil state bindings
   (:map evil-normal-state-map
         ("]b" . next-buffer)
         ("[b" . previous-buffer)
         ("]d" . flymake-goto-next-error)
         ("[d" . flymake-goto-prev-error)
         ("]h" . diff-hl-next-hunk)
         ("[h" . diff-hl-previous-hunk)
         ("g1" . avy-goto-char-timer)
         ("g2" . avy-goto-char-2)
         ("g3" . avy-goto-word-or-subword-1)
         ("gll" . avy-goto-line)
         ("glj" . avy-goto-line-below)
         ("glk" . avy-goto-line-above)
         ("gle" . avy-goto-end-of-line)
         ;; Unbind `evil-repeat-pop-next'
         ("M-." . nil))
   (:map evil-visual-state-map
         ("V" . expreg-contract)
         ("v" . expreg-expand))
   (:map evil-command-line-map
         ("C-a" . move-beginning-of-line)
         ("C-e" . move-end-of-line)
         ("C-f" . forward-char)
         ("C-b" . backward-char)
         ("C-d" . delete-char)
         ("C-k" . kill-line)
         ("C-o" . evil-command-window))))

(use-package evil-zh
  :hook (evil-mode . evil-zh-mode))

(use-package evil-surround
  :after evil

  :preface

  (defmacro my--quoted-text-object (name key start-regex end-regex)
    "Define text objects for `evil-mode'.

URL `https://stackoverflow.com/a/22418983/4921402'."
    (let ((inner-name (make-symbol (concat "evil-inner-" name)))
          (outer-name (make-symbol (concat "evil-a-" name))))
      `(progn
         (evil-define-text-object
           ,inner-name (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex
                              beg end type count nil))
         (evil-define-text-object
           ,outer-name (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex
                              beg end type count t))
         (keymap-set evil-inner-text-objects-map ,key #',inner-name)
         (keymap-set evil-outer-text-objects-map ,key #',outer-name))))

  :config

  (global-evil-surround-mode +1)

  ;; NOTE: do NOT use text-object such as `w', `p'
  (my--quoted-text-object "ShuMingHao" "q" "《" "》")
  (my--quoted-text-object "ShuangYinHao" "e" "“" "”")
  (my--quoted-text-object "DanYinHao" "d" "‘" "’")
  (my--quoted-text-object "ZhiJiaoYinHao" "r" "「" "」")
  (my--quoted-text-object "ZhiJiaoShuangYinHao" "f" "『" "』")
  (my--quoted-text-object "FangTouKuoHao" "t" "【" "】")
  (my--quoted-text-object "KongXinFangTouKuoHao" "g" "〖" "〗")
  (my--quoted-text-object "YuanKuoHao" "y" "（" "）")
  (my--quoted-text-object "QuanJiaoFangKuoHao" "u" "［" "］")
  (my--quoted-text-object "QuanJiaoWanKuoHao" "i" "〔" "〕")
  (my--quoted-text-object "QuanJiaoHuaKuoHao" "o" "｛" "｝")

  (add-hook 'org-mode-hook
            (lambda ()
              (dolist (alist '(
                               (?b . ("*" . "*"))
                               (?c . ("~" . "~"))
                               (?i . ("/" . "/"))
                               (?s . ("+" . "+"))
                               (?u . ("_" . "_"))
                               (?v . ("=" . "="))
                               ))
                (push alist evil-surround-pairs-alist))))

  (let ((alist '(
                 (?Q . ("《 " . " 》")) (?q . ("《" . "》"))
                 (?E . ("“ "  . " ”" )) (?e . ("“"  . "”" ))
                 (?D . ("‘ "  . " ’" )) (?d . ("‘"  . "’" ))
                 (?R . ("「 " . " 」")) (?r . ("「" . "」"))
                 (?F . ("『 " . " 』")) (?f . ("『" . "』"))
                 (?T . ("【 " . " 】")) (?t . ("【" . "】"))
                 (?G . ("〖 " . " 〗")) (?g . ("〖" . "〗"))
                 (?Y . ("（ " . " ）")) (?y . ("（" . "）"))
                 (?U . ("［ " . " ］")) (?u . ("［" . "］"))
                 (?I . ("〔 " . " 〕")) (?i . ("〔" . "〕"))
                 (?O . ("｛ " . " ｝")) (?o . ("｛" . "｝"))
                 )))
    (setq-default evil-surround-pairs-alist
                  (append alist evil-surround-pairs-alist))))

(use-package evil-nerd-commenter
  :after evil
  :bind (("C-c c l" . evilnc-comment-or-uncomment-lines)
         ("C-c c d" . evilnc-copy-and-comment-lines)
         ("C-c c p" . evilnc-comment-or-uncomment-paragraphs)
         (:map evil-normal-state-map
               ("gc" . evilnc-comment-operator)
               ("gp" . evilnc-copy-and-comment-operator)
               ("gy" . evilnc-yank-and-comment-operator))
         (:map evil-motion-state-map
               ("gc" . evilnc-comment-operator)
               ("gp" . evilnc-copy-and-comment-operator)
               ("gy" . evilnc-yank-and-comment-operator))))

;;; Template

(use-package tempel
  :preface
  (defun my--tempel-include (elt)
    "Add ELT (i template) to include templates by name in another template."
    (when (eq (car-safe elt) 'i)
      (if-let* ((template (alist-get (cadr elt) (tempel--templates))))
          (cons 'l template)
        (message "Template %s not found" (cadr elt))
        nil)))

  (defun my--tempel-setup-capf ()
    "Add the Tempel Capf to `completion-at-point-functions'.

Add before the Capfs, such that it will be tried first."
    ;; ;; Use a trigger prefix to prevent from triggering unexpectly
    ;; (setq-local corfu-auto-trigger "/"
    ;;             completion-at-point-functions
    ;;             (cons (cape-capf-trigger #'tempel-complete ?/)
    ;;                   completion-at-point-functions))

    ;; `tempel-expand' only triggers on exact matches, alternatively
    ;; use `tempel-complete' if you want to see all matches
    (setq-local completion-at-point-functions
                (cons #'tempel-expand completion-at-point-functions)))

  ;; Require trigger prefix before template name when completing
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete)
         ("M-_" . tempel-expand)
         ("M-*" . tempel-insert))

  :config

  (add-to-list 'tempel-user-elements #'my--tempel-include)

  :init

  (add-hook 'conf-mode-hook #'my--tempel-setup-capf)
  (add-hook 'prog-mode-hook #'my--tempel-setup-capf)
  (add-hook 'text-mode-hook #'my--tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  (global-tempel-abbrev-mode +1))

;;; Org

(use-package org
  :bind (("C-c o b" . org-switchb)
         ("C-c o d" . org-demote-subtree)
         ("C-c o i" . org-insert-structure-template)
         ("C-c o l" . org-store-link)
         ("C-c o o" . org-open-at-point-global)
         ("C-c o p" . org-promote-subtree)
         ("C-c o t" . org-toggle-link-display))

  :custom

  (org-agenda-files (list org-directory))
  (org-export-backends '(ascii beamer html latex md))

  ;; Respect property lines
  (org-startup-folded 'nofold)

  ;; Make Emacs respect kinsoku rules when wrapping lines visually
  (word-wrap-by-category t)

  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-src-fontify-natively t)
  (org-edit-src-content-indentation 0)

  ;; Save state changes in the LOGBOOK drawer
  (org-log-into-drawer t)

  ;; `X/Y', X means action when enters the state, Y means action when
  ;; leaves the state. Use `@' to add notes and status information
  ;; (including time), use `!' to add status information only

  ;; | DONE(d@)   | add notes when entering                            |
  ;; | DONE(d/!)  | add status when leaving                            |
  ;; | DONE(d@/!) | add note when entering and add status when leaving |
  ;; | DONE(d@/)  | WARNING: illegal                                   |

  ;; NOTE: When leaving state A to state B, if A has a leaving action
  ;; and B has an entering action. A's leaving action won't be triggered
  ;; instead of executing B's entering action
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
  ;; `{}' must exist to denote this is a subscript
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

;;;; Org preview
  ;; Enhance LaTeX preview in Org
  ;; https://kitchingroup.cheme.cmu.edu/blog/2016/11/06/Justifying-LaTeX-preview-fragments-in-org-mode/
  ;; Use center or right, anything else means left-justified as the default
  (plist-put org-format-latex-options :justify 'right)

  ;; ;; Enlarge the preview magnification
  ;; (plist-put org-format-latex-options :scale 1.5)

  (defun my--org-justify-fragment-overlay-h (beg end)
    "Adjust the justification of a LaTeX fragment horizontally.
The justification is set by :justify in `org-format-latex-options'.
Only equations at the beginning of a line are justified.

URL `https://kitchingroup.cheme.cmu.edu/blog/2016/11/06/Justifying-LaTeX-preview-fragments-in-org-mode/'."
    (let* ((position (plist-get org-format-latex-options :justify))
           (ov (car (overlays-at (/ (+ beg end) 2) t)))
           (width (car (image-size (overlay-get ov 'display))))
           offset)
      (cond
       ((and (eq 'center position)
             (= beg (line-beginning-position)))
        (setq offset (floor (- (/ fill-column 2)
                               (/ width 2))))
        (when (< offset 0)
          (setq offset 0))
        (overlay-put ov 'before-string (make-string offset #x20)))
       ((and (eq 'right position)
             (= beg (line-beginning-position)))
        (setq offset (floor (- fill-column width)))
        (when (< offset 0)
          (setq offset 0))
        (overlay-put ov 'before-string (make-string offset #x20))))))

  (advice-add 'org--make-preview-overlay
              :after #'my--org-justify-fragment-overlay-h)

  (defun my-org-toggle-justify-fragment-overlay-h ()
    "Toggle justify LaTeX fragment horizontally."
    (interactive)
    (if (advice-member-p #'my--org-justify-fragment-overlay-h
                         'org--make-preview-overlay)
        (advice-remove 'org--make-preview-overlay
                       #'my--org-justify-fragment-overlay-h)
      (advice-add 'org--make-preview-overlay
                  :after #'my--org-justify-fragment-overlay-h)))

  (defun my--org-justify-fragment-overlay-v (beg end)
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
    (let ((counter -1)
          results
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
                                   ;; `\\' is used for a new line
                                   ;; Each one leads to a number
                                   (cl-incf counter (count-matches "\\\\$"))
                                   ;; Unless there are nonumbers
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
      (advice-add 'org-create-formula-image :around #'my--org-renumber-fragment)))

;;;; Org timestamp
  ;; -----------------------------------------
  ;; C-c . \+1w RET ;; => <2020-05-23 Sat +1w>
  ;; C-c . \-1w RET ;; => <2020-05-23 Sat -1w>
  ;; -----------------------------------------
  (define-advice org-time-stamp (:around (fn &rest args) insert-escaped-repeater)
    "Insert escaped repeater for org timestamp."
    (apply fn args)
    (when (string-match (rx "\\" (group (any "+\\-") (0+ nonl)))
                        org-read-date-final-answer)
      (save-excursion
        (backward-char)
        (insert " "
                (string-trim-right
                 (match-string 1 org-read-date-final-answer)))))))

(use-package ob
  :defer t
  :custom (org-confirm-babel-evaluate nil)
  :config
  (define-advice org-babel-execute-src-block (:around (fn &rest args) lazy-load-languages)
    "Load languages when needed."
    (let* ((language (org-element-property :language (org-element-at-point)))
           (lang-cons (assoc (intern language) org-babel-load-languages)))
      (unless (cdr lang-cons)
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages
                                     org-babel-load-languages)))
    (apply fn args)))

(use-package ob-js
  :defer t
  :custom (org-babel-js-cmd "bun"))

(use-package ob-lisp
  :defer t
  :custom (org-babel-lisp-eval-fn #'sly-eval))

(use-package org-clock
  :defer t
  :custom
  ;; Save clock data and notes in the LOGBOOK drawer
  (org-clock-into-drawer t)
  ;; Remove clocked tasks with 0:00 duration
  (org-clock-out-remove-zero-time-clocks t))

(use-package org-archive
  :defer t
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
  :bind ("C-c o a" . org-agenda))

(use-package org-capture
  :preface

  (defun my--org-capture-find-month-tree ()
    "Go to current month heading."
    (let ((heading-list (string-split (format-time-string "%Y %m")))
          (level 1)
          end)
      (unless (derived-mode-p 'org-mode)
        (user-error "Target buffer `%s' should be in org mode"
                    (current-buffer)))
      (goto-char (point-min))
      ;; Locate YEAR headline, then MONTH headline
      (dolist (heading heading-list)
        (let ((re (format org-complex-heading-regexp-format
                          (regexp-quote heading))))
          (if (re-search-forward re end t)
              (goto-char (line-beginning-position))
            ;; Not found, create a new headline at EOF
            (goto-char (point-max))
            (or (bolp) (insert "\n"))
            (when (/= (point) (point-min)) (org-end-of-subtree t t))
            (insert (make-string level ?*) " " heading "\n")))
        (cl-incf level)
        (setq end (save-excursion (org-end-of-subtree t t))))
      (org-end-of-subtree)))

  :custom

  ;; |                org-capture-templates common used entry               |
  ;; |--------+-------------------------------------------------------------|
  ;; | %a     | annotation, normally the link created with `org-store-link' |
  ;; | %i     | initial content, copied from the active region              |
  ;; | %^g    | tag                                                         |
  ;; | %t     | timestamp, date only                                        |
  ;; | %T     | timestamp, with date and time                               |
  ;; | %u，%U | timestamp, but inactive                                     |
  ;; | %?     | cursor location after completing the template               |
  ;; NOTE: inactive timestamp will not be added to agenda

  (org-capture-templates
   '(("b" "Bill" plain
      (file+function "bill.org" my--org-capture-find-month-tree)
      "| %U | %^{category} | %^{desc} | %^{price} |" :kill-buffer t)
     ("c" "Capture" plain
      (file+olp+datetree org-default-notes-file)
      "* %^{capture}\n   %u\n")
     ("t" "Todo" entry
      (file+headline "todo.org" "Todo")
      "* TODO %^{todo}\n")
     ("w" "Work" entry
      (file+headline "work.org" "Work")
      "* %^{task name}\n   %t\n"
      :clock-in t :clock-resume t)
     ("r" "Read" entry
      (file+headline "read.org" "Book")
      "* %^{book name}\n   %u\n"
      :clock-in t :clock-resume t)))

  :bind ("C-c o c" . org-capture))

(use-package ox-latex
  :defer t
  :custom
  ;; Compared to `pdflatex', `xelatex' supports unicode and can use
  ;; system's font
  (org-latex-compiler "xelatex")
  ;; Export org in Chinese into PDF
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
  (setopt org-latex-default-class "ctexart"))

;;; Markup

(use-package toc-org
  :hook ((org-mode markdown-mode markdown-ts-mode) . toc-org-mode)
  :bind (:map markdown-mode-map
              :package markdown-mode
              ("C-c C-o" . toc-org-markdown-follow-thing-at-point)))

;; Pixel-perfect visual alignment for Org and Markdown tables
(use-package valign
  :if (display-graphic-p)
  :hook ((org-mode markdown-mode) . valign-mode)
  :config
  (defun my-valign-fancy-bar ()
    "Toggle valign fancy bar."
    (interactive)
    (setq valign-fancy-bar (not valign-fancy-bar)))

  ;; Compatible with `outline-mode'
  (define-advice outline-show-entry (:override nil)
    "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
    (interactive)
    (save-excursion
      (outline-back-to-heading t)
      (outline-flag-region (max (point-min) (1- (point)))
                           (progn
                             (outline-next-preface)
                             (if (= 1 (- (point-max) (point)))
                                 (point-max)
                               (point)))
                           nil))))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :bind (:map markdown-mode-command-map
              ("'" . markdown-edit-code-block)
              ("f" . markdown-footnote-goto-text)
              ("r" . markdown-footnote-return))
  :custom
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-italic-underscore t)
  (markdown-make-gfm-checkboxes-buttons t)

  (markdown-content-type "application/xhtml+xml")
  (markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
                        "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css"))
  (markdown-xhtml-header-content "
<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
<style>
  body {
    max-width: 50rem;
    width: 100%;
    margin: 2rem auto !important;
  }
</style>

<link rel='stylesheet' href='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/default.min.css'>
<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>
<script>
  document.addEventListener('DOMContentLoaded', () => {
    document.body.classList.add('markdown-body');
    document.querySelectorAll('pre code').forEach((code) => {
      if (code.className !== 'mermaid') {
        hljs.highlightElement(code);
      }
    });
  });
</script>

<script src='https://unpkg.com/mermaid/dist/mermaid.min.js'></script>
<script>
  mermaid.initialize({
    theme: 'default',  // default, forest, dark, neutral
    startOnLoad: true
  });
</script>
")
  (markdown-gfm-additional-languages "Mermaid")
  :hook (markdown-mode . (lambda ()
                           "The markdown files may contain tables, so do not wrap lines."
                           (setq truncate-lines t)))
  :config
  ;; `multimarkdown' is necessary for `highlight.js' and `mermaid.js'
  (when (executable-find "multimarkdown")
    (setq markdown-command "multimarkdown")))

(use-package typst-ts-mode
  :if (treesit-available-p)
  :custom (typst-ts-indent-offset 2)
  :mode "\\.typ\\'")

;;; Reader

(use-package elfeed
  :defer t
  :custom (url-queue-timeout 30))

(use-package elfeed-org
  :after elfeed
  :config (elfeed-org))

(use-package pdf-tools
  :if (display-graphic-p)
  :hook ((pdf-view-mode . pdf-isearch-minor-mode))
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode))

(use-package xiaoshuo-mode
  :bind ("C-c t x" . xiaoshuo-mode))

;;; Theme

(use-package modus-themes
  :defer t)

(use-package ef-themes
  :after modus-themes
  :defer t)

(use-package standard-themes
  :after modus-themes
  :defer t)

(use-package sinolor-themes
  :after modus-themes
  :defer t)

(use-package catppuccin-theme
  :defer t)

(use-package color-theme-sanityinc-tomorrow
  :defer t)

(use-package color-theme-sanityinc-solarized
  :defer t)

(use-package monokai-theme
  :defer t)

(use-package tao-theme
  :defer t)

(use-package zenburn-theme
  :defer t)

;;; Tequila worms

(use-package site-lisp)

(borg-report-init-duration)

(load (expand-file-name "~/.custom.el") t)

(message "Emacs ready in %s with %d garbage collections."
         (format "%.2f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time)))
         gcs-done)

;;; _
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
