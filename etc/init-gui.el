;;; init-gui.el --- init for gui -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; configuration for GUI Emacs.
;;

;;; Code:

;; ----- Frame ---------------------------------------------

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
  `(,user-full-name " @ " (:eval (if (buffer-file-name)
                                     (abbreviate-file-name (buffer-file-name))
                                   "%b"))))

;; Full screen when open GUI Emacs
(setq initial-frame-alist '((fullscreen . maximized)))

(use-package ns-auto-titlebar
  :when sys/mac-x-p
  :config (ns-auto-titlebar-mode +1))

;;; transparency
(defvar my-active-transparency 90
  "A value from the range (0..100), in increasing opacity.
Describes the transparency level of a frame when it's active or selected.
Transparency can be toggled through `toggle-transparency'.")

(defvar my-inactive-transparency 90
  "A value from the range (0..100), in increasing opacity.
Describes the transparency level of a frame when it's inactive or deselected.
Transparency can be toggled through `toggle-transparency'.")

(defvar my-current-opacity (frame-parameter (unless 'display-graphic-p) 'alpha)
  "Record current opacity.")

(defun my/toggle-transparency (&optional frame)
  "Toggle between transparent and opaque state for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let ((alpha (frame-parameter frame 'alpha))
        (setting (cons my-active-transparency
                       my-inactive-transparency)))
    (if (equal alpha setting)
        (my/disable-transparency frame)
      (my/enable-transparency frame setting))))

(defun my/enable-transparency (&optional frame alpha)
  "Enable transparency for FRAME.
If FRAME is nil, it defaults to the selected frame.
ALPHA is a pair of active and inactive transparency values.
The default value for ALPHA is based on
`my-active-transparency' and
`my-inactive-transparency'."
  (interactive)
  (let ((alpha-setting (or alpha
                           (cons my-active-transparency
                                 my-inactive-transparency))))
    (set-frame-parameter frame 'alpha alpha-setting)))

(defun my/disable-transparency (&optional frame)
  "Disable transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (set-frame-parameter frame 'alpha '(100 . 100)))

(defun my/set-transparency (value)
  "Set the VALUE of transparency of the frame window.
0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun my/increase-transparency (&optional frame)
  "Increase transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let* ((current-alpha (or (car (frame-parameter frame 'alpha)) 100))
         (increased-alpha (- current-alpha 5)))
    (when (>= increased-alpha frame-alpha-lower-limit)
      (set-frame-parameter frame 'alpha
        (cons increased-alpha increased-alpha)))))

(defun my/decrease-transparency (&optional frame)
  "Decrease transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let* ((current-alpha (or (car (frame-parameter frame 'alpha)) 100))
         (decreased-alpha (+ current-alpha 5)))
    (when (<= decreased-alpha 100)
      (set-frame-parameter frame 'alpha
        (cons decreased-alpha decreased-alpha)))))

(defun my/transient-transparency ()
  "Transient version of transparency."
  (interactive)
  (let ((echo-keystrokes nil))
    (message "Transparency: [s]et [t]oggle [-] [=]")
    (set-transient-map
      (let ((map (make-sparse-keymap)))
        (define-key map [?s] #'my/set-transparency)
        (define-key map [?t] #'my/toggle-transparency)
        (define-key map [?-] #'my/increase-transparency)
        (define-key map [?=] #'my/decrease-transparency)
        map)
      t)))

(global-set-key (kbd "C-c t p") #'my/transient-transparency)

;; ----- Font ----------------------------------------------

;; ;; https://archive.casouri.cat/note/2019/emacs-%E5%AD%97%E4%BD%93%E4%B8%8E%E5%AD%97%E4%BD%93%E9%9B%86/index.html
;; ;; http://ergoemacs.org/emacs/emacs_list_and_set_font.html
;; ;;
;; ;; Emacs use `symbola' (https://dn-works.com/ufas/) as default fallback font
;; ;; install it to avoid traversing all fonts
;; ;;
;; ;; NOTE: I am using `my/load-font' to handle this now
;; ;;
;; ;; Default font
;; (set-face-attribute 'default nil :font (font-spec :family "Unifont" :size 16))
;; ;;
;; ;; East Asia: 你好, 早晨, こんにちは, 안녕하세요
;; ;;
;; ;; ¯\_(ツ)_/¯
;; (dolist (charset '(han cjk-misc))
;;   (set-fontset-font t charset "Sarasa Mono Slab SC"))
;; (set-fontset-font t 'kana "Sarasa Mono Slab J")
;; (set-fontset-font t 'hangul "Sarasa Mono Slab K")

(defcustom my-font nil
  "Used to cache font configuration across sessions."
  :type 'string
  :group 'convenience)

(defvar my-font-alist
  '(
     (sarasa-mono-slab-sc-16 . (:family "Sarasa Mono Slab SC" :size 16))
     (sarasa-mono-sc-16      . (:family "Sarasa Mono SC" :size 16))
     (sarasa-gothic-sc-16    . (:family "Sarasa Gothic SC" :size 16))
     (sarasa-ui-sc-16        . (:family "Sarasa UI SC" :size 16))
     (gnu-unifont-16         . (:family "Unifont" :size 16))
     (sourcecodepro-nerd-14  . (:family "SauceCodePro Nerd Font" :size 14))
     (wenquanyi-16           . (:family "WenQuanYi Zen Hei Mono" :size 16))
     (anonymous-nerd-16      . (:family "Anonymice Nerd Font" :size 16))
     (blex-nerd-14           . (:family "BlexMono Nerd Font" :size 14))
     (caskaydia-nerd-14      . (:family "CaskaydiaCove Nerd Font" :size 14))
     (firacode-nerd-14       . (:family "FiraCode Nerd Font" :size 14))
     (hack-nerd-14           . (:family "Hack Nerd Font" :size 14))
     (jetbrainsmono-nerd-14  . (:family "JetBrainsMono Nerd Font" :size 14))
     (monaco-14              . (:family "Monaco" :size 14))
     (mononoki-nerd-16       . (:family "Mononoki Nerd Font" :size 16))
     (sf-mono-14             . (:family "SF Mono" :size 14))
     (victor-mono-nerd-14    . (:family "VictorMono Nerd Font" :size 14))
     (fangzheng-fangsong-16  . (:family "FZFangSong-Z02" :size 16))
     (fangzheng-heiti-16     . (:family "FZHei-B01" :size 16))
     (fangzheng-kaiti-16     . (:family "FZKai-Z03" :size 16))
     (fangzheng-shusong-16   . (:family "FZShuSong-Z01" :size 16))
     (fangzheng-pxys-16      . (:family "FZPingXianYaSong-R-GBK" :size 16))
     (fangzheng-gwfs-16      . (:family "FZDocFangSong" :size 16))
     (fangzheng-gwhw-16      . (:family "FZDocHei" :size 16))
     (fangzheng-gwkt-16      . (:family "FZDocKai" :size 16))
     (fangzheng-gwxbs-16     . (:family "FZDocXiaoBiaoSong" :size 16))
     (lantinghei-sc-16       . (:family "Lantinghei SC" :size 16))
     (lantinghei-tc-16       . (:family "Lantinghei TC" :size 16))
     (huawen-kaiti-sc-16     . (:family "Kaiti SC" :size 16))
     (huawen-kaiti-tc-16     . (:family "Kaiti TC" :size 16))
     (hiragino-sans-gb       . (:family "Hiragino Sans GB" :size 16))
     (hiragino-sans-cns      . (:family "Hiragino Sans CNS" :size 16))
     (source-han-sans-sc-16  . (:family "Source Han Sans SC" :size 16))
     (source-han-sans-tc-16  . (:family "Source Han Sans TC" :size 16))
     (source-han-serif-sc-16 . (:family "Source Han Serif SC" :size 16))
     (source-han-serif-tc-16 . (:family "Source Han Serif TC" :size 16))
     )
  "An alist of all the fonts you can switch between by `my/load-font'.
Key is a symbol as the name, value is a plist specifying the font spec.
More info about spec in `font-spec'.")

(defun my/load-buffer-font (&optional font-name)
  "Prompt for a FONT-NAME and set it for current buffer.
Fonts are specified in `my-font-alist'."
  (interactive (list
                 (completing-read "Choose a font for current buffer: "
                   (mapcar #'car my-font-alist))))
  (let* ((font-name (or font-name my-font))
         (font (apply #'font-spec
                 (if font-name
                     (alist-get (intern font-name) my-font-alist
                                nil nil #'equal)
                   (cdar my-font-alist)))))
    ;; use `face-remapping-alist' instead of `buffer-face-mode-face'
    (set (make-local-variable 'face-remapping-alist)
      (copy-tree `((default :font ,font
                            :height ,(* 10 (font-get `,font ':size))))))))

(global-set-key (kbd "C-c l f") #'my/load-buffer-font)

(defun my/load-font (&optional font-name)
  "Prompt for a FONT-NAME and set it.
Fonts are specified in `my-font-alist'.  If FONT-NAME non-nil,
use it instead."
  (interactive (list
                 (completing-read "Choose a font: "
                   (mapcar #'car my-font-alist))))
  (let* ((font-name (or font-name my-font))
         (font (apply #'font-spec
                 (if font-name
                     (alist-get (intern font-name) my-font-alist
                                nil nil #'equal)
                   ;; If font-name is nil (loading from local file and don't
                   ;; have it saved), use the first font spec.
                   (cdar my-font-alist)))))
    (set-frame-font font nil t)
    ;; seems that there isn't a good way to get font-object directly
    (add-to-list 'default-frame-alist
                 `(font . ,(face-attribute 'default :font)))
    (when (or font-name (not (custom-variable-p my-font)))
      (customize-set-variable 'my-font font-name))))

(global-set-key (kbd "C-c l F") #'my/load-font)

(my/load-font)

;; https://www.reddit.com/r/emacs/comments/988paa/emacs_on_windows_seems_lagging/
;; speed up font rendering for special characters, especially on Windows
(setq inhibit-compacting-font-caches t)

(provide 'init-gui)

;;; init-gui.el ends here
