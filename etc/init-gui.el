;;; init-gui.el --- init for gui -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; configuration for GUI.
;;

;;; Code:

;; ----- Frame ---------------------------------------------

(setq-default frame-title-format "GNU Emacs %@ %b")

;; Move more smoothly.
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode +1))

(when (featurep 'ns)
  ;; Make NS behavior the same as other platforms.
  (setq ns-command-modifier 'meta)
  (setq ns-alternate-modifier 'super)

  (defun my--set-frame-ns-titlebar (frame &rest _)
    "Set ns-appearance frame parameter for FRAME."
    (when (display-graphic-p frame)
      (let ((mode (frame-parameter frame 'background-mode)))
        (modify-frame-parameters
         frame
         `((ns-transparent-titlebar . t) (ns-appearance . ,mode))))))

  (defun my--set-all-frames-ns-titlebar (&rest _)
    "Set ns-appearance frame parameter for all frames."
    (mapc #'my--set-frame-ns-titlebar (frame-list)))

  (add-hook 'after-init-hook #'my--set-all-frames-ns-titlebar)
  (add-hook 'after-make-frame-functions #'my--set-frame-ns-titlebar)
  (advice-add 'frame-set-background-mode :after #'my--set-frame-ns-titlebar))

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
  (setq line-spacing space))

(keymap-global-set "C-c w l" #'my-set-line-spacing)

;; ----- Font ----------------------------------------------

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

(defvar my-font-settings nil
  "A list of (FACE . FONT-NAME).
FONT-NAMEs are keys in `my-font-alist'.")

(defvar my-cjk-rescale-alist
  '(("Source Han Serif SC" . 1.3)
    ("Source Han Sans SC" . 1.3))
  "A list of font names that should be rescaled.")

(defvar my-font-alist
  '(("霞鹜文楷等宽"      . ("LXGW WenKai Mono" nil 1))
    ("Unifont"           . ("Unifont" nil 1))
    ("Cascadia Code"     . ("Cascadia Code" "Sarasa Mono SC" 1))
    ("Cascadia Mono"     . ("Cascadia Mono" "Sarasa Mono SC" 1))
    ("Fira Code"         . ("Fira Code" "Sarasa Mono SC" 1))
    ("Hack"              . ("Hack" "Sarasa Mono Slab SC" 1))
    ("Jetbrains Mono"    . ("Jetbrains Mono" "Sarasa Mono SC" 1))
    ("Menlo"             . ("Menlo" "Sarasa Mono Slab SC" 1))
    ("Monaco"            . ("Monaco" "LXGW WenKai Mono" 1))
    ("Monego"            . ("Monego" "LXGW WenKai Mono" 1))
    ("Roboto Mono"       . ("Roboto Mono" "Sarasa Mono Slab SC" 1))
    ("Roboto"            . ("Roboto" "Sarasa Mono Slab SC" 1))
    ("SF Mono Light 14"  . ("SF Mono" "LXGW WenKai Mono" 1 :size 14 :weight light))
    ("SF Mono"           . ("SF Mono" "LXGW WenKai Mono" 1))
    ("Spot Mono"         . ("Spot Mono" "Sarasa Mono SC" 1))
    ("冬青黑体 简"       . ("Hiragino Sans GB" nil 1))
    ("冬青黑体 繁"       . ("Hiragino Sans CNS" nil 1))
    ("华文楷体 简"       . ("Kaiti SC" nil 1))
    ("华文楷体 繁"       . ("Kaiti TC" nil 1))
    ("思源宋体 简"       . ("Source Han Serif SC" nil 1))
    ("思源宋体 简 cjk"   . (nil "Source Han Serif SC" 1))
    ("思源宋体 繁"       . ("Source Han Serif TC" nil 1))
    ("思源宋体 繁 cjk"   . (nil "Source Han Serif TC" 1))
    ("思源黑体 简"       . ("Source Han Sans SC" nil 1))
    ("思源黑体 简 cjk"   . (nil "Source Han Sans SC" 1))
    ("思源黑体 繁"       . ("Source Han Sans TC" nil 1))
    ("思源黑体 繁 cjk"   . (nil "Source Han Sans TC" 1))
    ("文泉驿等宽正黑"    . ("WenQuanYi Zen Hei Mono" nil 1))
    ("方正书宋"          . ("FZShuSong Z01" nil 1))
    ("方正仿宋"          . ("FZFangSong Z02" nil 1))
    ("方正公文仿宋"      . ("FZDocFangSong" nil 1))
    ("方正公文小标宋"    . ("FZDocXiaoBiaoSong" nil 1))
    ("方正公文楷体"      . ("FZDocKai" nil 1))
    ("方正公文黑体"      . ("FZDocHei" nil 1))
    ("方正兰亭黑 简"     . ("Lantinghei SC" nil 1))
    ("方正兰亭黑 繁"     . ("Lantinghei TC" nil 1))
    ("方正屏显雅宋"      . ("FZPingXianYaSong R GBK" nil 1))
    ("方正楷体"          . ("FZKai Z03" nil 1))
    ("方正黑体"          . ("FZHei B01" nil 1))
    ("更纱黑体 Gothic"   . ("Sarasa Gothic SC" nil 1))
    ("更纱黑体 UI"       . ("Sarasa UI SC" nil 1))
    ("等距更纱黑体 Slab" . ("Sarasa Mono Slab SC" nil 1))
    ("等距更纱黑体"      . ("Sarasa Mono SC" nil 1))
    ("霞鹜文楷 cjk"      . (nil "LXGW WenKai" 1))
    ("霞鹜文楷"          . ("LXGW WenKai" nil 1)))
  "An alist of all the fonts you can switch between by `my-load-font'.
Each element is like

    (FONT-NAME . (ASCII-NAME CJK-NAME CJK-SCALE))

FONT-NAME is the display name, ASCII-NAME is the ASCII font
family name, CJK-NAME is the CJK font family name, CJK-SCALE is
the CJK font rescale ratio.")

(defun my--create-fontset (ascii-spec cjk-spec)
  "Create a fontset NAME with ASCII-SPEC and CJK-SPEC font."
  (let* ((fontset-name
          (concat "fontset-" (downcase (plist-get ascii-spec :family))))
         ;; ASCII font.
         (fontset
          (create-fontset-from-fontset-spec
           (font-xlfd-name
            (apply #'font-spec :registry fontset-name ascii-spec)))))
    ;; CJK font.
    (dolist (charset '(han kana hangul cjk-misc))
      (set-fontset-font fontset charset (apply #'font-spec cjk-spec)))
    fontset))

(defun my-font-name-to-spec (font-name size &rest attrs)
  "Translate FONT-NAME, SIZE and ATTRS to (ASCII-SPEC CJK-SPEC)."
  (let* ((font-spec (if (null font-name)
                        (cdar my-font-alist)
                      (alist-get font-name my-font-alist
                                 nil nil #'equal)))
         (ascii-family (nth 0 font-spec))
         (cjk-family (nth 1 font-spec))
         (cjk-scale (nth 2 font-spec))
         (rest-spec (append (nthcdr 3 font-spec) attrs))
         ;; (rest-spec (setf (plist-get rest-spec :size) size))
         (ascii-rest-spec (append `(:size ,size) rest-spec))
         (cjk-rest-spec (append `(:size ,(* cjk-scale size))
                                rest-spec))
         (ascii-spec (and ascii-family
                          `(:family ,ascii-family ,@ascii-rest-spec)))
         (cjk-spec (and cjk-family
                        `(:family ,cjk-family ,@cjk-rest-spec))))
    (list ascii-spec cjk-spec)))

(defun my-load-default-font (font-name size &rest attrs)
  "Set font for default face to FONT-NAME with SIZE and ATTRS.
See `my-load-font'."
  ;; We use a separate function for default font because Emacs has a
  ;; bug that prevents us from setting a fontset for the default face
  ;; (although `set-frame-parameter' works). So we just set default
  ;; face with ASCII font and use default fontset for Unicode font.
  (interactive
   (list (completing-read
          "Font: " (mapcar #'car my-font-alist))
         (string-to-number (completing-read
                            "Size: " nil nil nil nil nil
                            ;; Default value.
                            (cond
                             ;; 4k.
                             ((>= (display-pixel-width) 3840) "28")
                             ;; 2k.
                             ((>= (display-pixel-width) 2560) "20")
                             ;; 1080.
                             (t "14"))))))
  (let* ((specs (apply #'my-font-name-to-spec font-name size attrs))
         (ascii (apply #'font-spec (car specs)))
         (cjk (apply #'font-spec (cadr specs))))
    (set-face-attribute 'default nil :font ascii)
    (set-fontset-font t 'han cjk)
    (set-fontset-font t 'kana cjk)
    (set-fontset-font t 'hangul cjk)
    (set-fontset-font t 'cjk-misc cjk)
    (set-fontset-font t 'symbol cjk nil 'append)))

(keymap-global-set "C-c l f" #'my-load-default-font)

(defun my-load-font (face font-name size &rest attrs)
  "Set font for FACE to FONT-NAME.
If FONT-NAME is nil, use the first font in `my-font-alist'.
SIZE is the font size in pt. Add additional face attributes in
ATTRS.

Use `my-save-font-settings' to save font settings and use
`my-load-saved-font' to load them next time."
  (interactive
   (list (intern (completing-read
                  "Face: " (face-list)))
         (completing-read
          "Font: " (mapcar #'car my-font-alist))
         (string-to-number (completing-read
                            "Size: " nil nil nil nil nil
                            ;; default value
                            (cond
                             ;; 4k
                             ((>= (display-pixel-width) 3840) "28")
                             ;; 2k
                             ((>= (display-pixel-width) 2560) "20")
                             ;; 1080
                             (t "14"))))))
  (if (and (eq face 'default))
      (apply #'my-load-default-font font-name size attrs)
    (let* ((fontset
            (apply #'my--create-fontset
                   (apply #'my-font-name-to-spec font-name size attrs))))
      (apply #'set-face-attribute face nil
             :font fontset
             :fontset fontset
             attrs)))
  ;; Save the settings.
  (setf (alist-get face my-font-settings) `(,font-name ,size ,@attrs))
  (custom-set-variables
   `(my-font-settings
     ',my-font-settings
     nil nil "Automatically saved by `my-load-font'")))

(keymap-global-set "C-c l F" #'my-load-font)

(defun my-save-font-settings ()
  "Save font-settings set by `my-load-font'."
  (interactive)
  (custom-save-all))

(defun my-load-saved-font ()
  "Load font settings saved in `my-font-settings'."
  (interactive)
  (dolist (setting my-font-settings)
    (apply #'my-load-font setting)))

(define-minor-mode my-scale-cjk-mode
  "Scale CJK font to align CJK font and ASCII font."
  :lighter ""
  :global t
  :group 'convenience
  (dolist (setting my-cjk-rescale-alist)
    (setf (alist-get (car setting)
                     face-font-rescale-alist nil nil #'equal)
	  (if my-scale-cjk-mode (cdr setting) nil))))

(defun my-enable-apple-emoji ()
  "Enable Apple emoji display."
  (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji")
                    nil 'prepend))

(my-enable-apple-emoji)

(cond
 ;; 4k.
 ((>= (display-pixel-width) 3840)
  (my-load-font 'default nil 28))
 ;; 2k.
 ((>= (display-pixel-width) 2560)
  (my-load-font 'default nil 20))
 ;; 1080.
 (t
  (my-load-font 'default nil 14)))

;; https://www.reddit.com/r/emacs/comments/988paa/emacs_on_windows_seems_lagging/
;; Speed up font rendering for special characters, especially on Windows.
(setq inhibit-compacting-font-caches t)

(provide 'init-gui)

;;; init-gui.el ends here
