;;; init-dired.el --- dired configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Dired.
;;

;;; Code:

;; enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)

(with-eval-after-load 'dired

  ;; extra dired functionality
  (require 'dired-aux)

  ;; reuse current buffer by pressing `a'
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file)

  ;; search file name only when focus is over file
  (setq dired-isearch-filenames 'dwim)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies  'always)

  (when my-mac-p
    (if (executable-find "gls")
        ;; use GNU ls as `gls' from `coreutils' if available.
        (setq insert-directory-program "gls")
      ;; suppress the warning: `ls does not support --dired'.
      (setq dired-use-ls-dired nil)))

  ;; Show directory first
  ;; https://emacs.stackexchange.com/questions/5649/sort-file-names-numbered-in-dired/5650#5650
  (setq dired-listing-switches "-alhG1v --group-directories-first")

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  ;; https://blog.twonegatives.com/post/19292622546/dired-dwim-target-is-j00-j00-magic
  (setq dired-dwim-target t)

  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store\\'"
                "\\|^.project\\(?:ile\\)?\\'"
                "\\|^.\\(svn\\|git\\)\\'"
                "\\|^.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")))

(defun my-ediff-files ()
  "Inspired by https://oremacs.com/2017/03/18/dired-ediff/."
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (cond
     ((<= (length files) 2)
      (let ((file1 (car files))
            (file2 (if (cdr files)
                       (cadr files)
                     (read-file-name
                      "file: "
                      (dired-dwim-target-directory)))))
        (if (file-newer-than-file-p file1 file2)
            (ediff-files file2 file1)
          (ediff-files file1 file2))
        (add-hook 'ediff-after-quit-hook-internal
                  (lambda ()
                    (setq ediff-after-quit-hook-internal nil)
                    (set-window-configuration wnd)))))
     (t
      (error "No more than 2 files should be marked!")))))

(defun my-dired-cycle-space-underscore-hyphen ()
  "In Dired, rename current or marked files.
Cycling between space, hyphen - and underscore _.
If not in Dired, do nothing.

http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (progn
        (mapc (lambda (x)
                (let ((x (file-name-nondirectory x)))
                  (cond
                   ((string-match " " x)
                    (rename-file x
                                 (replace-regexp-in-string " " "-" x)
                                 nil))
                   ((string-match "-" x)
                    (rename-file x
                                 (replace-regexp-in-string "-" "_" x)
                                 nil))
                   ((string-match "_" x)
                    (rename-file x
                                 (replace-regexp-in-string "_" " " x)
                                 nil)))))
              (dired-get-marked-files))
        (revert-buffer))
    (user-error "Not in Dired!")))

(defun my--dired-mode-hook-setup ()
  "Setup for Dired."
  (local-set-key (kbd ",") #'dired-up-directory)
  (local-set-key (kbd "_") #'my-dired-cycle-space-underscore-hyphen)
  (local-set-key (kbd "C-c C-e") #'my-ediff-files)
  (local-set-key (kbd "C-c C-p") #'wdired-change-to-wdired-mode))

(add-hook 'dired-mode-hook #'my--dired-mode-hook-setup)

(provide 'init-dired)

;;; init-dired.el ends here
