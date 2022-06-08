;;; init-funcs.el --- useful functions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Define useful functions.
;;

;;; Code:

;;;;;;;;
;; VC ;;
;;;;;;;;

(defun my/vc-rename-file-and-buffer ()
  "Rename current buffer.
If current buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name
              (read-from-minibuffer "New name: " filename))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(global-set-key (kbd "C-c v r") #'my/vc-rename-file-and-buffer)

(defun my/vc-copy-file-and-rename-buffer ()
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

(global-set-key (kbd "C-c v c") #'my/vc-copy-file-and-rename-buffer)

(defun my/vc-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? "
                                filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(global-set-key (kbd "C-c v d") #'my/vc-delete-file-and-buffer)

;;;;;;;;;;;;
;; Window ;;
;;;;;;;;;;;;

(defun my/scroll-other-window-up ()
  "Scroll other window up forward."
  (interactive)
  (scroll-other-window '-))

(global-set-key (kbd "M-s M-j") #'scroll-other-window)
(global-set-key (kbd "M-s M-k") #'my/scroll-other-window-up)

(defun my/toggle-two-split-window ()
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

(global-set-key (kbd "C-c w t") #'my/toggle-two-split-window)

(defun my/rotate-windows ()
  "Rotate windows in clock-wise direction."
  (interactive)
  (cond
    ((not (> (count-windows) 1))
      (message "You can't rotate a single window!"))
    (t
      (let ((i 1) (numWindows (count-windows)))
        (while (< i numWindows)
          (let* ((w1 (elt (window-list) i))
                 (w2 (elt (window-list) (+ (% i numWindows) 1)))
                 (b1 (window-buffer w1))
                 (b2 (window-buffer w2))
                 (s1 (window-start w1))
                 (s2 (window-start w2)))
            (set-window-buffer w1 b2)
            (set-window-buffer w2 b1)
            (set-window-start w1 s2)
            (set-window-start w2 s1)
            (setq i (1+ i))))))))

(global-set-key (kbd "C-c w r") #'my/rotate-windows)

(defun my/toggle-full-window()
  "Toggle full view of selected window."
  (interactive)
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Splitting-Windows.html
  (if (window-parent)
      (delete-other-windows)
    (winner-undo)))

(global-set-key (kbd "C-c w f") #'my/toggle-full-window)

;;;;;;;;;;
;; FILE ;;
;;;;;;;;;;

(defun my//add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist'.
Use specified MODE for all given file PATTERNS."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun my//add-interpreter-mode (mode &rest patterns)
  "Add entries to `interpreter-mode-alist'.
Use specified MODE for all given file PATTERNS."
  (dolist (pattern patterns)
    (add-to-list 'interpreter-mode-alist (cons pattern mode))))

(defun my/revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Buffer reverted.")))

(global-set-key [remap revert-buffer] #'my/revert-this-buffer)

(defun my//update-files (&rest files)
  "Ensure FILES are updated in `recentf', `magit' and `save-place'."
  (let (toplevels)
    (dolist (file files)
      (when (featurep 'vc)
        (vc-file-clearprops file)
        (when-let (buffer (get-file-buffer file))
          (with-current-buffer buffer
            (vc-refresh-state))))
      (when (featurep 'magit)
        (when-let (default-directory (magit-toplevel
                                      (file-name-directory file)))
          (cl-pushnew default-directory toplevels)))
      (unless (file-readable-p file)
        (when (bound-and-true-p recentf-mode)
          (recentf-remove-if-non-kept file))))
    (dolist (default-directory toplevels)
      (magit-refresh))
    (when (bound-and-true-p save-place-mode)
      (save-place-forget-unreadable-files))))

(defun my/copy-this-file (new-path &optional force-p)
  "Copy current buffer's file to NEW-PATH.
If FORCE-P, overwrite the target file if it exists, without confirmation."
  (interactive
    (list (read-file-name "Copy file to: ")
          current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) 't)
    (copy-file old-path new-path (or force-p 1))
    (my//update-files old-path new-path)
    (message "File copied to %S" (abbreviate-file-name new-path))))

(global-set-key (kbd "C-c f c") #'my/copy-this-file)

(defun my/move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH.
If FORCE-P, overwrite the target file if it exists, without confirmation."
  (interactive
    (list (read-file-name "Move file to: ")
          current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) 't)
    (rename-file old-path new-path (or force-p 1))
    (set-visited-file-name new-path t t)
    (my//update-files old-path new-path)
    (message "File moved to %S" (abbreviate-file-name new-path))))

(global-set-key (kbd "C-c f m") #'my/move-this-file)

(defun my/rename-this-file (new-name)
  "Rename both current buffer and file to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name)))
  (save-buffer))

(global-set-key (kbd "C-c f r") #'my/rename-this-file)

(defun my/copy-file-name ()
  "Copy current buffer file name to clipboard."
  (interactive)
  (if-let ((filename (if (equal major-mode 'dired-mode)
                         default-directory
                       (buffer-file-name))))
    (cond
      ((equal current-prefix-arg nil)   ; no prefix
        (progn
          (kill-new (file-name-nondirectory filename))
          (message "Copied [%s]" (file-name-nondirectory filename))))
      (t                                ; others
        (progn
          (kill-new filename)
          (message "Copied [%s]" filename))))
    (message "WARNING: Current buffer is not attached to a file!")))

(global-set-key (kbd "C-c f y") #'my/copy-file-name)

(defun my/browse-this-file ()
  "Open current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file!")
      (browse-url (concat "file://" file-name)))))

(global-set-key (kbd "C-c f b") #'my/browse-this-file)

(defun my/open-this-file-externally (arg)
  "Open visited file in default external program.
When in Dired mode, open file under the cursor.
With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name
           (if (eq major-mode 'dired-mode)
               (dired-get-file-for-visit)
             buffer-file-name))
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (call-process program nil 0 nil current-file-name)))

(global-set-key (kbd "C-c f o") #'my/open-this-file-externally)

(defun my/delete-this-file ()
  "Delete current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete ‘%s’?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(global-set-key (kbd "C-c f d") #'my/delete-this-file)

(defun my/delete-file (file)
  "Delete FILE under current working directory."
  (interactive "sFile name: ")
  (shell-command
   (format "find . -depth -name %s -print0 | xargs -0 rm" file))
  (message "‘%s’ under current working directory deleted." file))

(global-set-key (kbd "C-c f D") #'my/delete-file)

(defun my//sudo-file-path (file)
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

(defun my/sudo-edit-file ()
  "Edit current file as root."
  (interactive)
  (find-file
    (my//sudo-file-path
      (or buffer-file-name
          (when (or (derived-mode-p 'dired-mode)
                    (derived-mode-p 'wdired-mode))
            default-directory)))))

(global-set-key (kbd "C-c f s") #'my/sudo-edit-file)

(defun my/sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (my//sudo-file-path file)))

(global-set-key (kbd "C-c f S") #'my/sudo-find-file)

(defun my/save-file-as-utf8 (coding-system)
  "Revert a buffer with CODING-SYSTEM and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(global-set-key (kbd "C-c f u") #'my/save-file-as-utf8)

(defun my/retab (arg &optional beg end)
  "Convert tabs-to-spaces or spaces-to-tabs within BEG and END.
Default to buffer start and end, to make indentation consistent.
Which it does depends on the value of `indent-tab-mode'.

If ARG (universal argument) is non-nil, retab the current buffer
using the opposite indentation style."
  (interactive "P\nr")
  (unless (and beg end)
    (setq beg (point-min)
          end (point-max)))
  (let ((indent-tabs-mode (if arg
                              (not indent-tabs-mode)
                            indent-tabs-mode)))
    (if indent-tabs-mode
        (tabify beg end)
      (untabify beg end))))

(defun my/dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun my/unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun my/toggle-indent-style ()
  "Switch between TAB/SPACE indentation style in current buffer."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "Indent style changed to %s."
           (if indent-tabs-mode "tabs" "spaces")))

;;;;;;;;;;;;;;
;; JUST4FUN ;;
;;;;;;;;;;;;;;

;; show ascii table
(defun my/ascii-table ()
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

(defun my/pingan-emacs ()
  "建议击毙, @平安 Emacs."
  (interactive)
  (insert
    (let ((list '()))
      (mapatoms (lambda (sym)
                  (when (special-form-p sym)
                    (push sym list))))
      (mapconcat (lambda (sym)
                   (format "%s 平安" sym))
        list
        ", "))))

;;;;;;;;;;;;;;;;;;;;
;; SEARCH RELATED ;;
;;;;;;;;;;;;;;;;;;;;

(defcustom my-search-engine nil
  "Used to cache search configuration across sessions."
  :type 'string
  :group 'convenience)

(defvar my-search-engine-alist
  '(
     (baidu          . "https://www.baidu.com/s?wd=")
     (bing           . "https://www.bing.com/search?q=")
     (duckduckgo     . "https://www.duckduckgo.com/?q=")
     (gitee          . "https://search.gitee.com/?q=")
     (github         . "https://www.github.com/search?q=")
     (google         . "https://www.google.com/search?q=")
     (stackoverflow  . "https://stackoverflow.com/search?q=")
     (vocabulary     . "https://www.vocabulary.com/dictionary/")
     (wikipedia      . "https://www.wikipedia.org/wiki/Special:Search?go=Go&search=")
     (youtube        . "https://www.youtube.com/results?search_query=")
     )
  "An alist of all the engines you can search by.
Key is a symbol as the name, value is a plist specifying the search url.")

(defun my/search-online (&optional search-engine)
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

(global-set-key (kbd "C-c s o") #'my/search-online)

;;;;;;;;;;;;;;;
;; DAILY USE ;;
;;;;;;;;;;;;;;;

;; a no-op function to bind to if you want to set a keystroke to null
(defun my/void () "This is a no-op." (interactive))

(defun my/run-cmd-and-replace-region (cmd)
  "Run CMD in shell on selected region or whole buffer.
And replace it with cli output."
  (let ((orig-point (point))
        (start (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (shell-command-on-region start end cmd nil t)
    (goto-char orig-point)))

(defun my/format-region-or-buffer(&optional column)
  "Perform a bunch of operations on the content of a buffer.
Indent region/buffer with COLUMN space(s).
Including indent-buffer, which should not be called automatically on save."
  (interactive "P")
  (save-excursion
    (if (region-active-p)
        (progn
          (delete-trailing-whitespace (region-beginning) (region-end))
          (indent-region (region-beginning) (region-end) column)
          (message "Selected region formatted."))
      (progn
        (delete-trailing-whitespace)
        (indent-region (point-min) (point-max) column)
        (message "Buffer formatted.")))))

(global-set-key (kbd "C-c m f") #'my/format-region-or-buffer)

;; https://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
(defun my/toggle-selective-display (column)
  "Quick and dirty code folding with COLUMN."
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 1))))

(global-set-key (kbd "C-c m h") #'my/toggle-selective-display)

;; https://www.emacswiki.org/emacs/RecreateScratchBuffer
(defun my/switch-scratch-buffer ()
  "Create or switch to a *scratch* buffer."
  (interactive)
  (if (get-buffer "*scratch*")
      (switch-to-buffer "*scratch*")
    (progn
      (switch-to-buffer (get-buffer-create "*scratch*"))
      (funcall initial-major-mode)
      (insert initial-scratch-message))))

(global-set-key (kbd "C-c X") #'my/switch-scratch-buffer)

(defun my/create-tags ()
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

(global-set-key (kbd "C-c m t") #'my/create-tags)

(defun my/occur-dwim ()
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
  (call-interactively 'occur))

(global-set-key (kbd "M-s o") #'my/occur-dwim)

(defun my/hide-dos-eol ()
  "Do not show  in files containing mixed UNIX and DOS line endings."
  (interactive)
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\ []))

(defun my/remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun my/switch-to-shell ()
  "Switch to built-in or 3rd party shell."
  (interactive)
  (cond
    ((display-graphic-p)
      (switch-to-builtin-shell))
    (t
      (suspend-frame))))

(global-set-key (kbd "C-c m z") #'my/switch-to-shell)

(defun my/load-theme (x)
  "Disable current theme and load theme X."
  (interactive (list
                 (completing-read "Choose a theme: "
                   (mapcar #'symbol-name (custom-available-themes)))))
  (condition-case nil
      (progn
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme (intern x) t))
    (error "Problem loading theme %s!" x)))

(defun my/emacs-default-theme ()
  "Load default Emacs theme."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(global-set-key (kbd "C-c m e") #'my/emacs-default-theme)

(defun my/kill-other-buffers-without-special-ones ()
  "Keep all buffers but the current one.
Do NOT mess with special buffers."
  (interactive)
  (when (y-or-n-p "Are you sure you want to kill all buffers but the current with special ones? ")
    (seq-each #'kill-buffer
      (delete (current-buffer) (seq-filter #'buffer-file-name (buffer-list))))))

(global-set-key (kbd "C-c m k") #'my/kill-other-buffers-without-special-ones)

;; https://stackoverflow.com/questions/3417438/closing-all-other-buffers-in-emacs
(defun my/kill-other-buffers-with-special-ones ()
  "Keep all buffers (include special buffers) but the current one."
  (interactive)
  (when (y-or-n-p "Are you sure you want to kill all buffers but the current one? ")
    (mapc 'kill-buffer (cdr (buffer-list (current-buffer))))))

(global-set-key (kbd "C-c m K") #'my/kill-other-buffers-with-special-ones)

(defun my/toggle-hl-line ()
  "Toggle function `global-hl-line-mode'."
  (interactive)
  (global-hl-line-mode 'toggle))

(global-set-key (kbd "C-c t h") #'my/toggle-hl-line)

(defun my/toggle-line-number ()
  "Show line numbers by `display-line-numbers-mode' (Emacs 26.1) or `linum-mode'."
  (interactive)
  (if (fboundp 'display-line-numbers-mode)
      (cond
        (display-line-numbers (display-line-numbers-mode -1))
        (t (display-line-numbers-mode +1)))
    (if (bound-and-true-p linum-mode)
        (linum-mode -1)
      (progn
        (linum-mode +1)
        (setq linum-format "%4d ")))))

(global-set-key (kbd "C-c t l") #'my/toggle-line-number)

(defun my/strfile2dat ()
  "Strfile current file to make it readable by `fortune'."
  (interactive)
  (let ((dat-file (concat (buffer-file-name) ".dat")))
    (shell-command (format "strfile %s %s" (buffer-file-name) dat-file))
    (message "Strfile finish: %s." dat-file)))

(defun my/insert-date (prefix)
  "Insert the ISO 8601 date format (like \"%+4Y-%m-%d\").
With one PREFIX, use ISO format.
With two PREFIX, use standard time format.
With three PREFIX, insert locale's timestamp."
  (interactive "P")
  (let ((format (cond
                  ((not prefix) "%F")
                  ((equal prefix '(4)) "%FT%H:%M:%S%:z")
                  ((equal prefix '(16)) "%s")
                  ((equal prefix '(64)) "%c"))))
    (insert (format-time-string format))))

(global-set-key (kbd "C-c 1") #'my/insert-date)

(defun my/insert-user-information (prefix)
  "Insert user information.
With one PREFIX, insert variable `user-full-name' only.
With two PREFIX, insert variable `user-mail-address' only."
  (interactive "P")
  (let ((format (cond
                  ((not prefix) (concat user-full-name " <"
                                        user-mail-address ">"))
                  ((equal prefix '(4)) user-mail-address)
                  ((equal prefix '(16)) user-full-name))))
    (insert format)))

(global-set-key (kbd "C-c 2") #'my/insert-user-information)

(defun my/divide-file-chapter ()
  "Divide FILE according to specified word."
  (interactive)
  (goto-char (point-min))
  (while (< (point) (point-max))
    (beginning-of-line)
    (if (search-forward-regexp "^第.\\{1,6\\}[回章话]" (line-end-position) t)
        (progn
          (end-of-line)
          (newline))
      (while (not (or (search-forward-regexp "^第.\\{1,6\\}[回章话]"
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
    (delete-blank-lines)
    (delete-blank-lines)
    (forward-line)))

(defun my/delete-visual-blank-lines ()
  "Delete all visual blank line."
  (interactive)
  (save-restriction
    (narrow-to-region (window-start) (window-end))
    (delete-matching-lines "^[ \t]*$" (point-min) (point-max))))

(global-set-key (kbd "C-c m d") #'my/delete-visual-blank-lines)

(defun my/fixup-whitespace ()
  "Fix up white space between objects around point.
Leave one space or none, according to the context."
  (interactive "*")
  (save-excursion
    (delete-horizontal-space)
    (if (or (looking-at "^\\|\\s)")
            (save-excursion (forward-char -1)
                            ;; we adapted the regexp here:
                            (looking-at "\\cc\\|$\\|\\s(\\|\\s'")))
        nil
      (insert ?\s))))

(defun my/delete-indentation (old-func &rest args)
  "My modified `delete-indentation'.
Fix OLD-FUNC with ARGS."
  (cl-letf (((symbol-function 'fixup-whitespace) #'my/fixup-whitespace))
    (apply old-func args)))

(advice-add 'delete-indentation :around #'my/delete-indentation)

(defun my/recompile-init ()
  "Byte-compile dotfiles again."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(defun my/indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(global-set-key (kbd "C-M-z") #'my/indent-defun)

;; https://emacs-china.org/t/emacs-builtin-mode/11937/63
;; press u undo and r to redo
(defun my/transient-winner-undo ()
  "Transient version of `winner-undo'."
  (interactive)
  (let ((echo-keystrokes nil))
    (my|ensure 'winner)
    (winner-undo)
    (message "Winner: [u]ndo [r]edo [q]uit")
    (set-transient-map
      (let
        ((map
           (make-sparse-keymap)))
        (define-key map
          [?u]
          #'winner-undo)
        (define-key map
          [?r]
          #'winner-redo)
        map)
      t)))

(global-set-key (kbd "C-x 4 u") #'my/transient-winner-undo)

(defun my//calculate-time-duration (start end)
  "Calculate seconds (format: SS) duration from START to END (format: \"HH:MM:SS\")."
  (-
    (let ((end-sum 0) (end-acc 0))
      (mapc
        (lambda (x)
          (if (and (<= (- x ?0) 9) (>= (- x ?0) 0))
              (setq end-acc (+ (* 10 end-acc) (- x ?0)))
            (setq end-sum (+ (* end-sum 60) end-acc)
                  end-acc 0)))
        end)
      (setq end-sum (+ (* end-sum 60) end-acc))
      end-sum)
    (let ((start-sum 0) (start-acc 0))
      (mapc
        (lambda (x)
          (if (and (<= (- x ?0) 9) (>= (- x ?0) 0))
              (setq start-acc (+ (* 10 start-acc) (- x ?0)))
            (setq start-sum (+ (* start-sum 60) start-acc)
                  start-acc 0)))
        start)
      (setq start-sum (+ (* start-sum 60) start-acc))
      start-sum)))

(defun my//convert-second-to-time-format (second)
  "Convert input SECOND(SS) to time format(\"HH:MM:SS\")."
  (let (time)
    (dotimes (_ 2)
      (setq time (concat (format ":%02d" (% second 60)) time)
            second (/ second 60)))
    (setq time
      (concat
        (format "%02d" second)
        time))
    time))

(defun my//adjust-point-after-click (event &optional _)
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
  (if my-delicate-click-mode
      (advice-add 'mouse-set-point :after #'my//adjust-point-after-click)
    (advice-remove 'mouse-set-point #'my//adjust-point-after-click)))

(defun my//add-subdirs-to-load-path (parent-dir)
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (let ((default-directory parent-dir))
    (setq load-path
          (append
           (cl-remove-if-not
            #'file-directory-p
            (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
           load-path))))

;; Network Proxy
(defun my/show-http-proxy ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is \"%s\"." my-http-proxy)
    (message "No HTTP proxy.")))

(defun my/enable-http-proxy ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,my-http-proxy)
          ("https" . ,my-http-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (my/show-http-proxy))

(defun my/disable-http-proxy ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (my/show-http-proxy))

(defun my/toggle-http-proxy ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (my/disable-http-proxy)
    (my/enable-http-proxy)))

(defun my/show-socks-proxy ()
  "Show SOCKS proxy."
  (interactive)
  (when (fboundp 'cadddr)               ; defined 25.2+
    (if (bound-and-true-p socks-noproxy)
        (message "Current SOCKS%d proxy is \"%s:%s\"."
                 (cadddr socks-server)
                 (cadr socks-server)
                 (caddr socks-server))
      (message "No SOCKS proxy."))))

(defun my/enable-socks-proxy ()
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
  (my/show-socks-proxy))

(defun my/disable-socks-proxy ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil
        socks-server nil)
  (setenv "all_proxy" "")
  (my/show-socks-proxy))

(defun my/toggle-socks-proxy ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (my/disable-socks-proxy)
    (my/enable-socks-proxy)))

(defun my/avy-copy-thing-at-point ()
  "Copy thing at point."
  (interactive)
  (save-excursion
    (avy-goto-word-or-subword-1)
    (let ((thing
           (cl-case
            (read-char
             (format
              "Copy thing at point (%s: word %s: symbol %s: list %s: url): "
              (propertize "w" 'face 'error)
              (propertize "s" 'face 'error)
              (propertize "l" 'face 'error)
              (propertize "u" 'face 'error)))
            (?w  'word)
            (?s  'symbol)
            (?l  'list)
            (?u  'url))))
      (kill-new (thing-at-point thing))
      (message "%s copied." thing))))

(global-set-key (kbd "C-c m c") #'my/avy-copy-thing-at-point)

(provide 'init-funcs)

;;; init-funcs.el ends here
