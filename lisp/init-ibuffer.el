;;; init-ibuffer.el --- ibuffer-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Use `ibuffer' to replace `list-buffers'.
;;

;;; Code:

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
  ;; Update filter group when calling `ibuffer'.
  (ibuffer . my--ibuffer-generate-filter-groups-by-major-mode)
  :config
  ;; Display vc status info in the ibuffer list.
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

  ;; Use human readable Size column instead of original one.
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

  (setopt ibuffer-formats
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
                                     ;; Trim `-mode' string.
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
                    ;; Created by major mode.
                    (cl-set-difference
                     (cl-remove-duplicates
                      (my--ibuffer-get-major-modes-list))
                     ignore-modes)
                    ;; Manually created.
                    '(("Modified" (predicate buffer-modified-p
                                             (current-buffer)))))))))
      (setopt ibuffer-saved-filter-groups groups)
      (ibuffer-switch-to-saved-filter-groups "default"))))

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
