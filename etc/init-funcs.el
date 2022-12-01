;;; init-funcs.el --- useful functions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Define useful functions.
;;

;;; Code:

;;;;;;;;
;; VC ;;
;;;;;;;;

(defun my-vc-rename-file-and-buffer ()
  "Rename current buffer.
If current buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-from-minibuffer "New name: " filename))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(global-set-key (kbd "C-c v r") #'my-vc-rename-file-and-buffer)

(defun my-vc-copy-file-and-rename-buffer ()
  "Copy the current buffer and file it is visiting.
If the old file is under version control, the new file is added into
version control automatically."
  (interactive)
  (let ((filename (buffer-file-name)))
    (cond
     ((not (and filename (file-exists-p filename)))
      (message "Buffer is not visiting a file!"))
     (t
      (let ((new-name (read-file-name "New name: " filename)))
        (copy-file filename new-name t)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)
        (when (vc-backend filename)
          (vc-register)))))))

(global-set-key (kbd "C-c v c") #'my-vc-copy-file-and-rename-buffer)

(defun my-vc-delete-file-and-buffer ()
  "Kill the current buffer and delete the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete `%s'? "
                                filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file `%s'." filename)
          (kill-buffer))))))

(global-set-key (kbd "C-c v d") #'my-vc-delete-file-and-buffer)

;;;;;;;;;;;;
;; Window ;;
;;;;;;;;;;;;

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
    (error "Not two windows in current frame!")))

(global-set-key (kbd "C-c w t") #'my-toggle-two-split-window)

(defun my-rotate-windows ()
  "Rotate windows in clock-wise direction."
  (interactive)
  (cond
   ((not (> (count-windows) 1))
    (message "You can't rotate a single window!"))
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
          (setq i (1+ i))))))))

(global-set-key (kbd "C-c w r") #'my-rotate-windows)

(defun my-toggle-full-window()
  "Toggle full view of selected window."
  (interactive)
  (if (window-parent)
      (delete-other-windows)
    (winner-undo)))

(global-set-key (kbd "C-c w f") #'my-toggle-full-window)

;;;;;;;;;;
;; FILE ;;
;;;;;;;;;;

(defun my-rename-this-file (&optional arg)
  "Rename both current buffer and file.
With a prefix ARG, rename based on current name."
  (interactive "P")
  (let ((filename (buffer-file-name)))
    (unless filename
      (error "Buffer `%s' is not visiting a file!" filename))
    (let ((new-name (read-string
                     "New name: "
                     (when arg (file-name-nondirectory filename)))))
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name +1))
        (set-visited-file-name new-name)
        (rename-buffer new-name)))
    (save-buffer)))

(global-set-key (kbd "C-c f r") #'my-rename-this-file)

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
      (warn "Current buffer is not attached to a file!"))))

(global-set-key (kbd "C-c f c") #'my-copy-file-name)

(defun my-browse-this-file ()
  "Open current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file!")
      (browse-url (concat "file://" file-name)))))

(global-set-key (kbd "C-c f b") #'my-browse-this-file)

(defun my-open-file-externally (file)
  "Open FILE externally using the default application of the system."
  (interactive "fOpen externally: ")
  (if (and (eq system-type 'windows-nt)
           (fboundp 'w32-shell-execute))
      (w32-shell-execute "open" file)
    (call-process (pcase system-type
                    ('darwin "open")
                    ('cygwin "cygstart")
                    (_ "xdg-open"))
                  nil 0 nil
                  (expand-file-name file))))

(global-set-key (kbd "C-c f o") #'my-open-file-externally)

(defun my-delete-this-file ()
  "Delete current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited!"))
  (when (yes-or-no-p (format "Really delete `%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(global-set-key (kbd "C-c f d") #'my-delete-this-file)

(defun my-delete-file (file)
  "Delete FILE under current working directory."
  (interactive "sFile name: ")
  (shell-command
   (format "find . -depth -name %s -print0 | xargs -0 rm" file))
  (message "`%s' under current working directory deleted." file))

(global-set-key (kbd "C-c f D") #'my-delete-file)

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

(global-set-key (kbd "C-c f s") #'my-sudo-edit-file)

(defun my-sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (my--sudo-file-path file)))

(global-set-key (kbd "C-c f S") #'my-sudo-find-file)

;;;;;;;;;;;;;;
;; JUST4FUN ;;
;;;;;;;;;;;;;;

(defun my-ascii-table ()
  "Print the ascii table."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
      (setq i (1+ i))
      (insert (format "%4d %c\n" i i))))
  (goto-char (point-min)))

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

;;;;;;;;;;;;;;;;;;;;
;; SEARCH RELATED ;;
;;;;;;;;;;;;;;;;;;;;

(defcustom my-search-engine nil
  "Used to cache search configuration across sessions."
  :type 'string
  :group 'convenience)

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
  (let* ((search-engine (or search-engine my-search-engine))
         (search-url (if search-engine
                         (alist-get (intern search-engine)
                                    my-search-engine-alist
                                    nil nil #'equal)
                       (cdar my-search-engine-alist)))
         (url search-url))
    (browse-url
     (url-encode-url
      (concat url
              (if mark-active
                  (buffer-substring (region-beginning) (region-end))
                (read-string
                 (message "%s Search: " (capitalize search-engine)))))))))

(global-set-key (kbd "C-c s o") #'my-search-online)

;;;;;;;;;;;;;;;
;; DAILY USE ;;
;;;;;;;;;;;;;;;

;; https://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
(defun my-toggle-selective-display (column)
  "Quick and dirty code folding with COLUMN."
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 1))))

(global-set-key (kbd "C-c m h") #'my-toggle-selective-display)

;; https://www.emacswiki.org/emacs/RecreateScratchBuffer
(defun my-switch-scratch-buffer ()
  "Create or switch to the *scratch* buffer."
  (interactive)
  (if (get-buffer "*scratch*")
      (switch-to-buffer "*scratch*")
    (progn
      (switch-to-buffer (get-buffer-create "*scratch*"))
      (funcall initial-major-mode)
      (insert initial-scratch-message))))

(global-set-key (kbd "C-c X") #'my-switch-scratch-buffer)

(defun my-switch-messages-buffer ()
  "Create or switch to the *Message* buffer."
  (interactive)
  (if (get-buffer "*Messages*")
      (switch-to-buffer "*Messages*")
    (progn
      (switch-to-buffer (get-buffer-create "*Messages*"))
      (messages-buffer-mode))))

(global-set-key (kbd "C-c M") #'my-switch-messages-buffer)

(defun my-create-tags ()
  "Create tags file."
  (interactive)
  (let* ((dir (read-directory-name "Ctags will scan code at: "))
         (default-directory dir)
         (name (read-string "Input tag file name (Default: tags): "
                            nil nil "tags"))
         (language
          (read-string "Input language (e.g.: c,c++,... Default: all): "
                       nil nil "all"))
         (extra-args (read-string "Input extra arguments (e.g.: -e): ")))
    (shell-command
     (format "ctags -f %s --languages=%s --kinds-all='*' --fields='*' --extras='*' %s -R %s"
             name language extra-args (directory-file-name dir)))
    (message "Tag file %s%s was created." dir name)))

(global-set-key (kbd "C-c m t") #'my-create-tags)

(defun my-occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively #'occur))

(global-set-key (kbd "C-c m o") #'my-occur-dwim)

(defun my-hide-dos-eol ()
  "Do not show  in files containing mixed UNIX and DOS line endings."
  (interactive)
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\ []))

(defun my-remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun my-load-theme (x)
  "Disable current theme and load theme X."
  (interactive (list
                (completing-read
                 "Choose a theme: "
                 (mapcar #'symbol-name (custom-available-themes)))))
  (condition-case nil
      (progn
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme (intern x) t))
    (error "Problem loading theme %s!" x)))

(global-set-key (kbd "C-c m l") #'my-load-theme)

(defun my-load-default-theme ()
  "Load default Emacs theme."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(global-set-key (kbd "C-c m L") #'my-load-default-theme)

(defun my-kill-other-buffers-without-special-ones ()
  "Keep all buffers but the current one.
Do NOT mess with special buffers."
  (interactive)
  (when (y-or-n-p "Are you sure you want to kill all buffers but the current with special ones? ")
    (seq-each #'kill-buffer
              (delete
               (current-buffer)
               (seq-filter #'buffer-file-name (buffer-list))))))

(global-set-key (kbd "C-c m k") #'my-kill-other-buffers-without-special-ones)

;; https://stackoverflow.com/questions/3417438/closing-all-other-buffers-in-emacs
(defun my-kill-other-buffers-with-special-ones ()
  "Keep all buffers (include special buffers) but the current one."
  (interactive)
  (when (y-or-n-p "Are you sure you want to kill all buffers but the current one? ")
    (mapc #'kill-buffer (cdr (buffer-list (current-buffer))))))

(global-set-key (kbd "C-c m K") #'my-kill-other-buffers-with-special-ones)

(defun my-strfile2dat ()
  "Strfile current file to make it readable by `fortune'."
  (interactive)
  (let ((dat-file (concat (buffer-file-name) ".dat")))
    (shell-command (format "strfile %s %s" (buffer-file-name) dat-file))
    (message "Strfile finish: %s." dat-file)))

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

(global-set-key (kbd "C-c 1") #'my-insert-date)

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

(global-set-key (kbd "C-c 2") #'my-insert-user-information)

(defun my-divide-file-chapter ()
  "Divide FILE according to specified word."
  (interactive)
  (goto-char (point-max))
  ;; make sure final newline exist
  (unless (bolp)
    (newline))
  (goto-char (point-min))
  (while (< (point) (point-max))
    (beginning-of-line)
    (if (re-search-forward "^第.\\{1,6\\}[回章话]" (line-end-position) t)
        (progn
          (end-of-line)
          (newline))
      (while (not (or (re-search-forward "^第.\\{1,6\\}[回章话]"
                                         (line-end-position) t)
                      (= (point) (point-max))))
        (forward-line))
      (forward-line -1)
      (end-of-line)
      (newline 2)
      (forward-line)
      (end-of-line)))
  (when (= (point) (point-max))
    (forward-line -1)
    ;; remove all blank lines at eof
    (delete-blank-lines)
    (delete-blank-lines)
    (forward-line)))

(defun my-delete-blank-lines ()
  "Delete blank lines.
When region is active, delete the blank lines in region only."
  (interactive)
  (if (region-active-p)
      (delete-matching-lines "^[[:space:]]*$" (region-beginning) (region-end))
    (delete-matching-lines "^[[:space:]]*$" (point-min) (point-max))))

(global-set-key (kbd "C-c m d") #'my-delete-blank-lines)

(defun my-delete-visual-blank-lines ()
  "Delete all visual blank lines."
  (interactive)
  (save-restriction
    (narrow-to-region (window-start) (window-end))
    (delete-matching-lines "^[[:space:]]*$" (point-min) (point-max))))

(global-set-key (kbd "C-c m D") #'my-delete-visual-blank-lines)

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

(defun my-fixup-whitespace ()
  "Add Chinese characters support for `fixup-whitespace'."
  (interactive "*")
  (save-excursion
    (delete-horizontal-space)
    (if (or (looking-at "^\\|\\s)")
            (save-excursion (forward-char -1)
                            (looking-at "\\cc\\|$\\|\\s(\\|\\s'")))
        nil
      (insert ?\s))))

;; use `cl-lef' to change the behavior of `fixup-whitespace' only when
;; called from `delete-indentation'
(defun my--delete-indentation (old-func &rest args)
  "My modified `delete-indentation'.
Fix OLD-FUNC with ARGS."
  (cl-letf (((symbol-function 'fixup-whitespace) #'my-fixup-whitespace))
    (apply old-func args)))

(advice-add 'delete-indentation :around #'my--delete-indentation)

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

(global-set-key (kbd "C-M-z") #'my-indent-defun)

(defun my--adjust-point-after-click (event &optional _)
  "Adjust point.  Click more accurate in line with intuition.
Adjust point depending on which portion of the character the
cursor clicked on, if on the right half, move point after.
EVENT is the mouse event."
  (let* ((posn (event-end event))
         (x (car (posn-object-x-y posn)))
         (w (car (posn-object-width-height posn))))
    ;; `mouse-set-point' is called twice when you click mouse
    ;; first in `down-mouse-1', called by `mouse-drag-region' ->
    ;; `mouse-drag-track' to set point, second in `mouse-1', when
    ;; mouse released and Emacs realized that this is a click event.
    ;; We want to adjust point in both cases.
    (when (and (null (posn-object posn))
               (> x (/ w 2))
               (not (eq (char-after) ?\n)))
      (forward-char))))

(define-minor-mode my-delicate-click-mode
  "Accurate point position on click.
That is, if you click on the right half of a character, the point
is set to after it."
  :global t
  :lighter ""
  :group 'convenience
  (if my-delicate-click-mode
      (advice-add 'mouse-set-point :after #'my--adjust-point-after-click)
    (advice-remove 'mouse-set-point #'my--adjust-point-after-click)))

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
1 and group 2 will both be non nil when a pangu space is needed.")

(defun my-pangu-spacing-current-buffer ()
  "Pangu space current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward my-pangu-spacing-regexp nil t)
      (when (and (match-beginning 1)
                 (match-beginning 2))
        (replace-match "\\1 \\2" nil nil)
        (backward-char))))
  ;; nil must be returned to allow use in hooks
  nil)

(global-set-key (kbd "C-c m p") #'my-pangu-spacing-current-buffer)

(defun my--add-subdirs-to-load-path (search-dir)
  "Add every subdir of SEARCH-DIR to `load-path'."
  (let ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             ;; filter out unnecessary dirs
             (cl-remove-if
              (lambda (subdir)
                (or
                 ;; remove if not directory
                 (not (file-directory-p (concat dir subdir)))
                 ;; remove dirs such as parent dir, programming language
                 ;; related dir and version control dir.
                 (member subdir '("." ".."
                                  "dist" "node_modules" "__pycache__"
                                  "RCS" "CVS" "rcs" "cvs"
                                  ".git" ".github"))))
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        (when (cl-some (lambda (subdir-file)
                         (and (file-regular-p
                               (concat subdir-path subdir-file))
                              ;; .so .dll are Emacs dynamic library
                              ;; written in non emacs-lisp
                              (member (file-name-extension subdir-file)
                                      '("el" "so" "dll"))))
                       (directory-files subdir-path))
          (add-to-list 'load-path subdir-path t))
        ;; recursively search subdirectories
        (my--add-subdirs-to-load-path subdir-path)))))

;; Network Proxy
(defun my-show-http-proxy ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is \"%s\"." my-http-proxy)
    (message "No HTTP proxy.")))

(defun my-enable-http-proxy ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,my-http-proxy)
          ("https" . ,my-http-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (my-show-http-proxy))

(defun my-disable-http-proxy ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (my-show-http-proxy))

(defun my-toggle-http-proxy ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (my-disable-http-proxy)
    (my-enable-http-proxy)))

(global-set-key (kbd "C-c t p h") #'my-toggle-http-proxy)
(global-set-key (kbd "C-c t p H") #'my-show-http-proxy)

(defun my-show-socks-proxy ()
  "Show SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (message "Current SOCKS%d proxy is \"%s:%s\"."
               (cadddr socks-server)
               (cadr socks-server)
               (caddr socks-server))
    (message "No SOCKS proxy.")))

(defun my-enable-socks-proxy ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost"))
  (let* ((proxy (split-string my-socks-proxy ":"))
         (host (car proxy))
         (port (cadr  proxy)))
    (setq socks-server `("Default server" ,host ,port 5)))
  (setenv "all_proxy" (concat "socks5://" my-socks-proxy))
  (my-show-socks-proxy))

(defun my-disable-socks-proxy ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil
        socks-server nil)
  (setenv "all_proxy" "")
  (my-show-socks-proxy))

(defun my-toggle-socks-proxy ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (my-disable-socks-proxy)
    (my-enable-socks-proxy)))

(global-set-key (kbd "C-c t p s") #'my-toggle-socks-proxy)
(global-set-key (kbd "C-c t p S") #'my-show-socks-proxy)

(defun my-show-wsl-socks-proxy ()
  "Show SOCKS proxy in WSL."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (message "Current SOCKS%d proxy in WSL is \"%s:%s\"."
               (cadddr socks-server)
               (cadr socks-server)
               (caddr socks-server))
    (message "No SOCKS proxy in WSL.")))

(defun my-enable-wsl-socks-proxy ()
  "Enable SOCKS proxy in WSL."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost"))
  (let* ((proxy (split-string my-wsl-socks-proxy ":"))
         (host (car proxy))
         (port (cadr  proxy)))
    (setq socks-server `("Default server" ,host ,port 5)))
  (setenv "all_proxy" (concat "socks5://" my-wsl-socks-proxy))
  (my-show-wsl-socks-proxy))

(defun my-disable-wsl-socks-proxy ()
  "Disable SOCKS proxy in WSL."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil
        socks-server nil)
  (setenv "all_proxy" "")
  (my-show-wsl-socks-proxy))

(defun my-toggle-wsl-socks-proxy ()
  "Toggle SOCKS proxy in WSL."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (my-disable-wsl-socks-proxy)
    (my-enable-wsl-socks-proxy)))

(global-set-key (kbd "C-c t p w") #'my-toggle-wsl-socks-proxy)
(global-set-key (kbd "C-c t p W") #'my-show-wsl-socks-proxy)

(provide 'init-funcs)

;;; init-funcs.el ends here
