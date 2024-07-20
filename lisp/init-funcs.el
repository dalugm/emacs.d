;;; init-funcs.el --- useful functions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Define useful functions.
;;

;;; Code:

;;;; Window.

(defun my-toggle-two-split-window ()
  "Toggle two window layout vertically or horizontally."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
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

(keymap-global-set "C-c w t" #'my-toggle-two-split-window)

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

(keymap-global-set "C-c w r" #'my-rotate-windows)

(defun my-toggle-full-window()
  "Toggle full view of selected window."
  (interactive)
  (if (window-parent)
      (delete-other-windows)
    (winner-undo)))

(keymap-global-set "C-c w f" #'my-toggle-full-window)

;;;; File.

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

(keymap-global-set "C-c f r" #'my-rename-this-file)

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

(keymap-global-set "C-c f c" #'my-copy-file-name)

(defun my-browse-this-file ()
  "Open current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (user-error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(keymap-global-set "C-c f b" #'my-browse-this-file)

(defun my-open-file-externally (file)
  "Open FILE externally using the default application of the system."
  (interactive "fOpen externally: ")
  (if (and my-win-p (fboundp 'w32-shell-execute))
      (w32-shell-execute "open" file)
    (call-process (pcase system-type
                    ('darwin "open")
                    ('cygwin "cygstart")
                    (_ "xdg-open"))
                  nil 0 nil
                  (expand-file-name file))))

(keymap-global-set "C-c f o" #'my-open-file-externally)

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

(keymap-global-set "C-c f d" #'my-delete-this-file)

(defun my-delete-file (file)
  "Delete FILE under current working directory."
  (interactive "sFile name: ")
  (shell-command
   (format "find . -depth -name %s -print0 | xargs -0 rm" file))
  (message "`%s' under current working directory deleted." file))

(keymap-global-set "C-c f D" #'my-delete-file)

(defun my--sudo-file-path (file)
  "Get current FILE's path."
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let (user (file-remote-p file 'user))
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

(keymap-global-set "C-c f s" #'my-sudo-edit-file)

(defun my-sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (my--sudo-file-path file)))

(keymap-global-set "C-c f S" #'my-sudo-find-file)

;;;; Search.

(defvar my-search-engine-alist
  '(
    (baidu         . "https://www.baidu.com/s?wd=")
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
    (zhihu         . "https://www.zhihu.com/search?type=content&q=")
    )
  "An alist of all the engines you can search by.
Key is a symbol as the name, value is a plist specifying the search url.")

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

(keymap-global-set "C-c s o" #'my-search-online)

;;;; Daily.

;; https://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
(defun my-toggle-selective-display (column)
  "Quick and dirty code folding with COLUMN."
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 1))))

(keymap-global-set "C-c m h" #'my-toggle-selective-display)

(defun my-occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (use-region-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively #'occur))

(keymap-global-set "C-c m o" #'my-occur-dwim)

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

(keymap-global-set "C-c m t" #'my-load-theme)

(defun my-load-default-theme ()
  "Load default Emacs theme."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(keymap-global-set "C-c m T" #'my-load-default-theme)

(defun my-kill-other-buffers-without-special-ones ()
  "Keep all buffers but the current one.
Do NOT mess with special buffers."
  (interactive)
  (when (y-or-n-p "Kill all buffers but current with special ones? ")
    (seq-do #'kill-buffer
            (remove
             (current-buffer)
             (seq-filter #'buffer-file-name (buffer-list))))))

(keymap-global-set "C-c m k" #'my-kill-other-buffers-without-special-ones)

;; https://stackoverflow.com/questions/3417438/closing-all-other-buffers-in-emacs
(defun my-kill-other-buffers-with-special-ones ()
  "Keep all buffers (include special buffers) but the current one."
  (interactive)
  (when (y-or-n-p "Kill all buffers but current one? ")
    (mapc #'kill-buffer (cdr (buffer-list (current-buffer))))))

(keymap-global-set "C-c m K" #'my-kill-other-buffers-with-special-ones)

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

(keymap-global-set "C-c m 1" #'my-insert-date)

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

(keymap-global-set "C-c m 2" #'my-insert-user-information)

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

(keymap-global-set "C-c m d" #'my-delete-blank-lines)

(defun my-delete-visual-blank-lines ()
  "Delete all visual blank lines."
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (window-start) (window-end))
      (delete-matching-lines "^[[:space:]]*$"
                             (point-min)
                             (point-max)))))

(keymap-global-set "C-c m D" #'my-delete-visual-blank-lines)

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

(defun my-recompile-init ()
  "Byte-compile dotfiles again."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(defun my-indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(keymap-global-set "C-M-z" #'my-indent-defun)

(defcustom my-pangu-spacing-excluded-puncuation
  "，。！？、；：‘’“”『』「」【】（）《》"
  "Excluded puncuation when pangu spacing buffer."
  :group 'convenience
  :type 'string)

(defvar my-pangu-spacing-regexp
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
1 and group 2 will both be non-nil when a pangu space is needed.")

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

(keymap-global-set "C-c m p" #'my-pangu-spacing-current-buffer)

;;;; Proxy.

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

(keymap-global-set "C-c t p h" #'my-toggle-http-proxy)
(keymap-global-set "C-c t p H" #'my-show-http-proxy)

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

(keymap-global-set "C-c t p s" #'my-toggle-socks-proxy)
(keymap-global-set "C-c t p S" #'my-show-socks-proxy)

;;;; JUST4FUN.

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

(provide 'init-funcs)
;;; init-funcs.el ends here
