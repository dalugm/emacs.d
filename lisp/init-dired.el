;;; init-dired.el --- dired configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Dired.
;;

;;; Code:

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
    "In Dired, rename current or marked files.
Cycling between space, hyphen - and underscore _.  If not in
Dired, do nothing.

URL `http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html'."
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
      (user-error "Not in Dired")))

  (defun my-dired-open-externally (&optional arg)
    "Open marked or current file in OS's default application."
    (interactive "P")
    (dired-map-over-marks
     (my-open-file-externally (dired-get-file-for-visit))
     arg))
  :custom
  (dired-listing-switches "-alh")
  ;; Search file name only when focus is over filename.
  (dired-isearch-filenames 'dwim)
  ;; Kill current dired buffer when selecting a new directory.
  (dired-kill-when-opening-new-dired-buffer t)
  ;; Make dired "guess" target directory.
  (dired-dwim-target t))

(provide 'init-dired)
;;; init-dired.el ends here
