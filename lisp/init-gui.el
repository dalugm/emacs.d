;;; init-gui.el --- init for gui -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; configuration for GUI.
;;

;;; Code:

;;;; Frame.

(setopt frame-title-format "GNU Emacs %@ %b")

;; Move more smoothly.
(pixel-scroll-precision-mode +1)

(when (featurep 'ns)
  ;; Make NS behavior the same as other platforms.
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
  (advice-add 'frame-set-background-mode :after #'my--set-frame-ns-appearance))

(defun my-set-window-transparency (value)
  "Set the VALUE of transparency of the frame window."
  (interactive "nSet transparency (0 is transparent - 100 is opaque): ")
  (set-frame-parameter (selected-frame) 'alpha value))

(keymap-global-set "C-c w p" #'my-set-window-transparency)

(defun my-set-window-margins (margin)
  "Set the MARGIN of the current window."
  (interactive "nMargin Value: ")
  (set-window-margins (selected-window) margin margin))

(keymap-global-set "C-c w m" #'my-set-window-margins)

(defun my-set-line-spacing (space)
  "Set the line SPACE of the current window."
  (interactive "nLine Space: ")
  (setopt line-spacing space))

(keymap-global-set "C-c w l" #'my-set-line-spacing)

;;;; Font.

;; ;; https://archive.casouri.cat/note/2019/emacs-%E5%AD%97%E4%BD%93%E4%B8%8E%E5%AD%97%E4%BD%93%E9%9B%86/index.html
;; ;; http://ergoemacs.org/emacs/emacs_list_and_set_font.html
;; ;;
;; ;; Emacs use `symbola' (https://dn-works.com/ufas/) as the default
;; ;; fallback font. Install to avoid traversing all fonts.
;; ;;
;; ;; NOTE: I am using `my-load-font' to handle this now.
;; ;;
;; ;; Default font.
;; (set-face-attribute 'default nil :font (font-spec :family "Unifont" :size 16))
;; ;;
;; ;; East Asia: 你好，こんにちは，안녕하세요。
;; ;;
;; ;; ¯\_(ツ)_/¯
;; (dolist (charset '(han cjk-misc))
;;   (set-fontset-font t charset "LXGW WenKai Mono"))
;; (set-fontset-font t 'kana "LXGW WenKai Mono")
;; (set-fontset-font t 'hangul "LXGW WenKai Mono")

(defvar my-font-alist
  '(("霞鹜文楷等宽" "LXGW WenKai Mono" nil 1)
    ("3270" "3270 Nerd Font Mono" "Unifont" 1)
    ("BigBlue Terminal" "BigBlueTermPlus Nerd Font Mono" "Unifont" 1)
    ("Comic" "ComicShannsMono Nerd Font" "LXGW WenKai Mono" 1)
    ("Fira Code" "FiraCode Nerd Font" "Sarasa Mono SC" 1)
    ("Hack" "Hack Nerd Font" "Sarasa Mono SC" 1)
    ("Jetbrains Mono" "JetbrainsMono Nerd Font" "Sarasa Mono SC" 1)
    ("Meslo" "MesloLGS Nerd Font Mono" "Sarasa Mono SC" 1)
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
additional font spec for ASCII and CJK font.")

(defun my--create-fontset (ascii-spec cjk-spec)
  "Create a fontset NAME with ASCII-SPEC and CJK-SPEC font."
  (let* ((font-hash (sxhash (list ascii-spec cjk-spec)))
         ;; If two fontset have the same ASCII spec and different CJK
         ;; spec, the fontset description is the same, we need to
         ;; differentiate between the two, hence the hash.
         (fontset-name
          (format "fontset-%s+%x"
                  (downcase (plist-get ascii-spec :family))
                  ;; Don't want negative sign ("-").
                  (abs font-hash)))
         ;; ASCII font.
         (fontset
          (create-fontset-from-fontset-spec
           (font-xlfd-name
            (apply #'font-spec :registry fontset-name ascii-spec)))))
    ;; CJK font.
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
         (cjk-family (nth 1 font-spec))
         (cjk-scale (nth 2 font-spec))
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

(defun my--font-default-size ()
  "Return the default font size."
  (cond
   ((>= (display-pixel-height) 2160) 28)
   ((>= (display-pixel-height) 1440) 20)
   (t 14)))

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
             ;; and non-ascii spec to take effect.
             :font fontset
             :fontset fontset
             attrs))))

(defun my-load-default-font (font-name size &rest attrs)
  "Load FONT-NAME for default face with SIZE and ATTRS.

More details are inside `my-load-font'."
  ;; We use a separate function for default font because Emacs has a
  ;; bug that prevents us from setting a fontset for the default face
  ;; (although `set-frame-parameter' works). So we just set default
  ;; face with ASCII font and use default fontset for Unicode font.
  (interactive
   (list (completing-read "Font: " (mapcar #'car my-font-alist) nil t)
         (read-number "Size: " (my--font-default-size))))
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

(keymap-global-set "C-c m f" #'my-load-default-font)

(defun my-load-font (face font-name size &rest attrs)
  "Load FONT-NAME for FACE with SIZE and ATTRS.

If FONT-NAME is nil, use the first font in `my-font-alist'.
SIZE is the font size in pt.  Add additional face attributes in
ATTRS."
  (interactive
   (list (intern (completing-read "Face: " (face-list) nil t))
         (completing-read "Font: " (mapcar #'car my-font-alist) nil t)
         (read-number "Size: " (my--font-default-size))))
  (let* ((spec (my--font-name-to-spec font-name))
         (fontset (apply #'my--create-fontset
                         (my--font-expand-spec spec size))))
    (if (eq face 'default)
        (apply #'my-load-default-font font-name size attrs)
      (apply #'set-face-attribute face nil
             :font fontset
             :fontset fontset
             attrs))))

(keymap-global-set "C-c m F" #'my-load-font)

(my-load-font 'default nil (my--font-default-size))

;; Emoji display.
(set-fontset-font t 'emoji
                  (font-spec
                   :family (concat
                            (if my-mac-p "Apple" "Noto")
                            " Color Emoji"))
                  nil 'prepend)

;; https://www.reddit.com/r/emacs/comments/988paa/emacs_on_windows_seems_lagging/
;; Speed up font rendering for special characters, especially on Windows.
(setq inhibit-compacting-font-caches t)

(provide 'init-gui)
;;; init-gui.el ends here
